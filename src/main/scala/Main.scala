import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*

import java.io.FileInputStream
import java.util.concurrent.{Executors, LinkedBlockingQueue}
import scala.concurrent.ExecutionContext

/** Application entry point.
 *
 *  Architecture: Model-View-Update (MVU) loop.
 *   1. A JVM thread reads raw characters from /dev/tty and places Msg
 *      values into a `LinkedBlockingQueue`.
 *   2. A bridging IO fiber drains that queue and forwards Msgs to the
 *      Cats Effect `Queue`, keeping all unsafe calls off the CE pool.
 *   3. The main loop dequeues Msgs, runs Update.apply, renders, then
 *      executes the resulting Cmd as a background IO fiber.
 *
 *  Platform note: Terminal raw mode relies on `stty` and `/dev/tty`, which
 *  are Unix-specific.  The application will fall back gracefully on
 *  environments without a TTY (e.g. CI), but interactive input will not work.
 */
object Main extends IOApp:

  private def dbConfigFromEnv: DbConfig = DbConfig(
    host     = sys.env.getOrElse("DB_HOST",     "localhost"),
    port     = sys.env.get("DB_PORT").flatMap(_.toIntOption).getOrElse(3306),
    user     = sys.env.getOrElse("DB_USER",     "root"),
    password = sys.env.getOrElse("DB_PASSWORD", ""),
    database = sys.env.get("DB_NAME")
  )

  def run(args: List[String]): IO[ExitCode] =
    // Wrap the blocking ExecutionContext in a Resource so it is shut down on exit.
    val blockingPool: Resource[IO, ExecutionContext] = Resource.make(
      IO(Executors.newFixedThreadPool(4))
    )(pool => IO(pool.shutdown())).map(ExecutionContext.fromExecutorService)

    blockingPool.flatMap(ec => DbService.resource(dbConfigFromEnv, ec)).use { db =>
      Queue.unbounded[IO, Msg].flatMap { ceQueue =>
        val rawQueue = new LinkedBlockingQueue[Msg]()
        for
          modelRef    <- Ref[IO].of(AppModel())
          _           <- ceQueue.offer(Msg.Init)
          // Raw input thread (plain JVM thread — no CE involvement)
          inputThread  = startInputThread(rawQueue)
          // Bridge fiber: drain rawQueue → ceQueue without unsafeRunSync
          bridgeFib   <- bridgeLoop(rawQueue, ceQueue).start
          result      <- uiLoop(db, ceQueue, modelRef)
          _           <- bridgeFib.cancel
          _           <- IO(inputThread.interrupt()).handleError(_ => ())
          _           <- IO(print(Ansi.ShowCursor))
        yield result
      }
    }.handleErrorWith { err =>
      IO(System.err.println(s"Fatal: ${err.getMessage}")).as(ExitCode.Error)
    }

  // ── Raw Input Thread (plain JVM — no Cats Effect) ────────────────
  /** Reads raw bytes from /dev/tty and deposits `Msg` values into a
   *  `LinkedBlockingQueue` so the CE world never needs `unsafeRunSync`.
   */
  private def startInputThread(raw: LinkedBlockingQueue[Msg]): Thread =
    val thread = new Thread(() => {
      setupRawMode()
      try
        val tty = new FileInputStream("/dev/tty")
        val buf = new Array[Byte](4)
        var running = true
        while running && !Thread.currentThread().isInterrupted do
          val n = tty.read(buf)
          if n <= 0 then running = false
          else
            val bytes = buf.take(n)
            msgFromBytes(n, bytes).foreach(raw.put)
      catch
        case _: InterruptedException => ()
        case ex: Exception =>
          System.err.println(s"[input] Error reading terminal: ${ex.getMessage}")
      finally
        restoreCookedMode()
    }, "tui-input")
    thread.setDaemon(true)
    thread.start()
    thread

  private def setupRawMode(): Unit =
    try Runtime.getRuntime.exec(Array("sh", "-c", "stty raw -echo </dev/tty")).waitFor()
    catch case ex: Exception =>
      System.err.println(s"[input] Warning: could not set raw terminal mode: ${ex.getMessage}")

  private def restoreCookedMode(): Unit =
    try Runtime.getRuntime.exec(Array("sh", "-c", "stty sane </dev/tty")).waitFor()
    catch case ex: Exception =>
      System.err.println(s"[input] Warning: could not restore terminal mode: ${ex.getMessage}")

  private def msgFromBytes(n: Int, bytes: Array[Byte]): Option[Msg] =
    (n, bytes(0).toInt) match
      case (1, 27)  => Some(Msg.SearchCancel)     // ESC
      case (1, 13)  => Some(Msg.ConfirmInput)     // Enter (CR)
      case (1, 10)  => Some(Msg.ConfirmInput)     // Enter (LF)
      case (1, 127) => Some(Msg.BackspaceInput)   // DEL
      case (1, 8)   => Some(Msg.BackspaceInput)   // BS
      case (1, 19)  => Some(Msg.CommitEdit)       // Ctrl+S
      case (3, _) if bytes(0) == 27.toByte && bytes(1) == '['.toByte =>
        bytes(2).toChar match
          case 'A' => Some(Msg.MoveUp)
          case 'B' => Some(Msg.MoveDown)
          case 'C' => Some(Msg.MoveRight)
          case 'D' => Some(Msg.MoveLeft)
          case _   => None
      case (1, ch) =>
        ch.toChar match
          case 'j'  => Some(Msg.MoveDown)
          case 'k'  => Some(Msg.MoveUp)
          case 'h'  => Some(Msg.MoveLeft)
          case 'l'  => Some(Msg.MoveRight)
          case 'q'  => Some(Msg.Quit)
          case '/'  => Some(Msg.StartSearch)
          case 'e'  => Some(Msg.StartEdit)
          case 'c'  => Some(Msg.SwitchView(View.QueryConsole))
          // Only route printable ASCII characters to avoid spurious control sequences.
          case c if c >= ' ' => Some(Msg.CharInput(c))
          case _             => None
      case _ => None

  // ── Bridge Loop ──────────────────────────────────────────────────
  /** Bridges the blocking `LinkedBlockingQueue` to the Cats Effect queue
   *  using `IO.blocking` (safe: consumes a thread from the blocking pool).
   */
  private def bridgeLoop(raw: LinkedBlockingQueue[Msg], ce: Queue[IO, Msg]): IO[Unit] =
    IO.blocking(raw.take())
      .flatMap(ce.offer)
      .foreverM

  // ── UI Loop ──────────────────────────────────────────────────────
  private def uiLoop(
    db:       DbService[IO],
    queue:    Queue[IO, Msg],
    modelRef: Ref[IO, AppModel]
  ): IO[ExitCode] =
    def step: IO[Boolean] =
      queue.take.flatMap { msg =>
        modelRef.get.flatMap { model =>
          val (newModel, cmd) = Update(model, msg)
          modelRef.set(newModel) *>
            IO(print(Ansi.HideCursor + Renderer.render(newModel))).handleError(_ => ()) *>
            executeCommand(db, cmd, queue, newModel).as(msg != Msg.Quit)
        }
      }

    def loop: IO[ExitCode] =
      step.flatMap {
        case true  => loop
        case false => IO.pure(ExitCode.Success)
      }

    IO(print(Ansi.HideCursor)) *> loop

  // ── Command Execution ────────────────────────────────────────────
  /** Execute a `Cmd` as a background Cats Effect fiber, offering results
   *  back to the message queue.
   */
  private def executeCommand(db: DbService[IO], cmd: Cmd, queue: Queue[IO, Msg], model: AppModel): IO[Unit] =
    val cfg = dbConfigFromEnv
    cmd match
      case Cmd.None => IO.unit
      case Cmd.Ping =>
        db.ping
          .map(ok => if ok then Msg.Connected(s"${cfg.host}:${cfg.port}") else Msg.DbError("Could not connect to database"))
          .handleError(e => Msg.DbError(e.getMessage))
          .flatMap(queue.offer)
          .start.void
      case Cmd.LoadSchemas =>
        db.listSchemas
          .map(Msg.SchemasLoaded.apply)
          .handleError(e => Msg.DbError(e.getMessage))
          .flatMap(queue.offer)
          .start.void
      case Cmd.LoadTables(schema) =>
        db.listTables(schema)
          .map(Msg.TablesLoaded.apply)
          .handleError(e => Msg.DbError(e.getMessage))
          .flatMap(queue.offer)
          .start.void
      case Cmd.LoadPrimaryKeys(schema, table) =>
        db.primaryKeys(schema, table)
          .map(Msg.PrimaryKeysLoaded.apply)
          .handleError(_ => Msg.PrimaryKeysLoaded(Nil))
          .flatMap(queue.offer)
          .start.void
      case Cmd.LoadData(schema, table, limit, offset) =>
        db.fetchTableData(schema, table, limit, offset)
          .map { case (cols, rows) => Msg.DataLoaded(cols, rows) }
          .handleError(e => Msg.DbError(e.getMessage))
          .flatMap(queue.offer)
          .start.void
      case Cmd.UpdateCell(schema, table, pkConditions, col, value) =>
        db.updateCell(schema, table, pkConditions, col, value)
          .map(Msg.CellUpdated.apply)
          .handleError(e => Msg.DbError(e.getMessage))
          .flatMap(queue.offer)
          .start.void
      case Cmd.RunQuery(sql) =>
        db.runQuery(sql)
          .map { case (cols, rows) => Msg.QueryResult(cols, rows) }
          .handleError(e => Msg.DbError(e.getMessage))
          .flatMap(queue.offer)
          .start.void
