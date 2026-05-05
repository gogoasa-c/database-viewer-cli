import cats.effect.{IO, Resource}
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.hikari.HikariTransactor

import java.sql.ResultSet
import scala.concurrent.ExecutionContext

/** Database connection configuration. */
final case class DbConfig(
  host:     String,
  port:     Int,
  user:     String,
  password: String,
  database: Option[String] = None
)

/** A single row from a query result — column name to string value. */
final case class Row(cells: Map[String, String])

/** Metadata about a schema (database). */
final case class SchemaInfo(name: String, tableCount: Int)

/** Algebra (tagless-final) for all database operations. */
trait DbService[F[_]]:
  def ping: F[Boolean]
  def listSchemas: F[List[SchemaInfo]]
  def listTables(schema: String): F[List[String]]
  /** Returns the primary-key column name(s) for the given table, or Nil if none. */
  def primaryKeys(schema: String, table: String): F[List[String]]
  def fetchTableData(schema: String, table: String, limit: Int, offset: Int): F[(List[String], List[Row])]
  /** Update a single cell, identified by a map of pkColumn → pkValue pairs (supports composite PKs). */
  def updateCell(schema: String, table: String, pkConditions: Map[String, String], column: String, value: String): F[Int]
  def runQuery(sql: String): F[(List[String], List[Row])]

object DbService:

  /** Validates that an identifier (schema/table/column name) contains only
   *  safe characters: letters, digits, and underscores.  Backtick quoting
   *  provides most protection, but this adds a defense-in-depth check.
   */
  private def validateIdentifier(name: String, kind: String): Either[String, String] =
    if name.matches("[\\w]+") then Right(name)
    else Left(s"Unsafe $kind name '$name': only [A-Za-z0-9_] are allowed")

  private def requireIdentifier(name: String, kind: String): IO[String] =
    IO.fromEither(validateIdentifier(name, kind).left.map(msg => new IllegalArgumentException(msg)))

  /** Build a HikariCP-backed transactor wrapped in a Resource for safe lifecycle. */
  def resource(cfg: DbConfig, blockingEc: ExecutionContext): Resource[IO, DbService[IO]] =
    val jdbcUrl =
      cfg.database.fold(
        s"jdbc:mysql://${cfg.host}:${cfg.port}/?allowPublicKeyRetrieval=true&useSSL=false"
      )(db => s"jdbc:mysql://${cfg.host}:${cfg.port}/$db?allowPublicKeyRetrieval=true&useSSL=false")

    HikariTransactor.newHikariTransactor[IO](
      driverClassName = "com.mysql.cj.jdbc.Driver",
      url             = jdbcUrl,
      user            = cfg.user,
      pass            = cfg.password,
      connectEC       = blockingEc
    ).map(xa => new LiveDbService(xa))

  /** Read all rows from a ResultSet into (columns, rows). */
  private def rsToData(rs: ResultSet): (List[String], List[Row]) =
    val meta     = rs.getMetaData
    val colCount = meta.getColumnCount
    val cols     = (1 to colCount).map(meta.getColumnName).toList
    val builder  = List.newBuilder[Row]
    while rs.next() do
      val cells = cols.zipWithIndex.map { case (col, i) =>
        val v = rs.getString(i + 1)
        col -> (if v == null then "NULL" else v)
      }.toMap
      builder += Row(cells)
    (cols, builder.result())

  /** Concrete implementation backed by a Doobie transactor. */
  private final class LiveDbService(xa: Transactor[IO]) extends DbService[IO]:

    def ping: IO[Boolean] =
      sql"SELECT 1".query[Int].unique.transact(xa).attempt.map(_.isRight)

    def listSchemas: IO[List[SchemaInfo]] =
      val q = sql"""
        SELECT s.schema_name, COUNT(t.table_name)
        FROM information_schema.schemata s
        LEFT JOIN information_schema.tables t
          ON t.table_schema = s.schema_name
        WHERE s.schema_name NOT IN ('information_schema','performance_schema','mysql','sys')
        GROUP BY s.schema_name
        ORDER BY s.schema_name
      """.query[(String, Int)].map { case (n, c) => SchemaInfo(n, c) }.to[List]
      q.transact(xa)

    def listTables(schema: String): IO[List[String]] =
      sql"""
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = $schema
        ORDER BY table_name
      """.query[String].to[List].transact(xa)

    def primaryKeys(schema: String, table: String): IO[List[String]] =
      sql"""
        SELECT column_name
        FROM information_schema.key_column_usage
        WHERE table_schema = $schema
          AND table_name   = $table
          AND constraint_name = 'PRIMARY'
        ORDER BY ordinal_position
      """.query[String].to[List].transact(xa)

    def fetchTableData(schema: String, table: String, limit: Int, offset: Int): IO[(List[String], List[Row])] =
      for
        s      <- requireIdentifier(schema, "schema")
        t      <- requireIdentifier(table,  "table")
        // Schema and table names are backtick-quoted identifiers; limit/offset are JDBC ? params.
        sql     = s"SELECT * FROM `$s`.`$t` LIMIT ? OFFSET ?"
        result <- HC.prepareStatement(sql)(
                    HPS.set(1, limit) *> HPS.set(2, offset) *> HPS.executeQuery(FRS.raw(rsToData))
                  ).transact(xa)
      yield result

    def updateCell(schema: String, table: String, pkConditions: Map[String, String], column: String, value: String): IO[Int] =
      for
        s      <- requireIdentifier(schema, "schema")
        t      <- requireIdentifier(table,  "table")
        col    <- requireIdentifier(column, "column")
        // Validate each PK column name; preserve key order by using the validated name to look up value.
        pkCols <- pkConditions.keys.toList.traverse(c => requireIdentifier(c, "pk column"))
        // Build: UPDATE `s`.`t` SET `col` = ? WHERE `pk1` = ? AND `pk2` = ? ...
        safeSchema  = Fragment.const(s"`$s`")
        safeTable   = Fragment.const(s"`$t`")
        safeCol     = Fragment.const(s"`$col`")
        whereFrags  = pkCols.map { c =>
                        // Look up value by key to avoid relying on Map.values iteration order.
                        Fragment.const(s"`$c`") ++ fr"= ${pkConditions(c)}"
                      }
        whereClause = whereFrags.reduce(_ ++ fr"AND" ++ _)
        q = (fr"UPDATE" ++ safeSchema ++ fr"." ++ safeTable ++
             fr"SET" ++ safeCol ++ fr"= $value WHERE" ++ whereClause).update
        n <- q.run.transact(xa)
      yield n

    def runQuery(rawSql: String): IO[(List[String], List[Row])] =
      HC.prepareStatement(rawSql)(HPS.executeQuery(FRS.raw(rsToData))).transact(xa)
