// TuiComponents: MVU model, message ADT, update function, and ANSI renderer.
// ─────────────────────────────────────────────────────────────────
//  ANSI helpers
// ─────────────────────────────────────────────────────────────────
object Ansi:
  val Reset        = "\u001b[0m"
  val Bold         = "\u001b[1m"
  val Dim          = "\u001b[2m"
  val ClearScreen  = "\u001b[2J\u001b[H"
  val HideCursor   = "\u001b[?25l"
  val ShowCursor   = "\u001b[?25h"

  // Foreground colours (256-colour)
  def fg(n: Int)  = s"\u001b[38;5;${n}m"
  def bg(n: Int)  = s"\u001b[48;5;${n}m"

  // Named palette (chosen to work on both dark & light terminals)
  val Cyan    = fg(51)
  val Green   = fg(82)
  val Yellow  = fg(226)
  val Red     = fg(196)
  val Blue    = fg(39)
  val Magenta = fg(201)
  val White   = fg(255)
  val Gray    = fg(245)
  val DarkBg  = bg(235)

  def colored(c: String, s: String): String = s"$c$s$Reset"
  def bold(s: String): String               = s"$Bold$s$Reset"
  def header(s: String): String             = s"$Bold$Cyan$s$Reset"

// ─────────────────────────────────────────────────────────────────
//  Domain Model
// ─────────────────────────────────────────────────────────────────
enum View:
  case SchemaBrowser
  case DataViewer
  case QueryConsole

enum EditState:
  case Idle
  case EditingCell(value: String)

final case class AppModel(
  // connection
  connected:       Boolean             = false,
  connStatus:      String              = "Disconnected",
  // navigation
  activeView:      View                = View.SchemaBrowser,
  schemas:         List[SchemaInfo]    = Nil,
  selectedSchema:  Option[String]      = None,
  tables:          List[String]        = Nil,
  selectedTable:   Option[String]      = None,
  // list cursor
  cursor:          Int                 = 0,
  // data viewer
  columns:         List[String]        = Nil,
  pkColumns:       List[String]        = Nil,  // primary key column(s) for the active table
  rows:            List[Row]           = Nil,
  dataOffset:      Int                 = 0,
  dataLimit:       Int                 = 50,
  selectedRow:     Int                 = 0,
  selectedCol:     Int                 = 0,
  editState:       EditState           = EditState.Idle,
  // query console
  queryInput:      String              = "",
  queryColumns:    List[String]        = Nil,
  queryRows:       List[Row]           = Nil,
  // search
  searching:       Boolean             = false,
  searchInput:     String              = "",
  // status
  statusMsg:       String              = "Press ? for help",
)

// ─────────────────────────────────────────────────────────────────
//  Message ADT
// ─────────────────────────────────────────────────────────────────
enum Msg:
  // Lifecycle
  case Init
  case Quit
  // Navigation
  case MoveUp
  case MoveDown
  case MoveLeft
  case MoveRight
  case Select
  case Back
  // Views
  case SwitchView(v: View)
  // Context-aware raw character input — routed by Update based on current view/state.
  case CharInput(c: Char)
  case BackspaceInput
  case ConfirmInput   // Enter key
  // Search
  case StartSearch
  case SearchInput(c: Char)
  case SearchBackspace
  case SearchConfirm
  case SearchCancel
  // Edit
  case StartEdit
  case EditInput(c: Char)
  case EditBackspace
  case CommitEdit
  case CancelEdit
  // Query console
  case QueryInput(c: Char)
  case QueryBackspace
  case ExecuteQuery
  // DB results (async)
  case SchemasLoaded(schemas: List[SchemaInfo])
  case TablesLoaded(tables: List[String])
  case PrimaryKeysLoaded(pkCols: List[String])
  case DataLoaded(columns: List[String], rows: List[Row])
  case CellUpdated(affectedRows: Int)
  case QueryResult(columns: List[String], rows: List[Row])
  case DbError(msg: String)
  case Connected(status: String)

// ─────────────────────────────────────────────────────────────────
//  Commands — IO actions that produce a Msg
// ─────────────────────────────────────────────────────────────────
enum Cmd:
  case None
  case LoadSchemas
  case LoadTables(schema: String)
  case LoadPrimaryKeys(schema: String, table: String)
  case LoadData(schema: String, table: String, limit: Int, offset: Int)
  /** pkConditions maps pk-column-name → pk-value; supports composite PKs. */
  case UpdateCell(schema: String, table: String, pkConditions: Map[String, String], col: String, value: String)
  case RunQuery(sql: String)
  case Ping

// ─────────────────────────────────────────────────────────────────
//  Update — pure MVU function
// ─────────────────────────────────────────────────────────────────
object Update:
  def apply(model: AppModel, msg: Msg): (AppModel, Cmd) = msg match

    case Msg.Init =>
      (model, Cmd.Ping)

    case Msg.Connected(s) =>
      (model.copy(connected = true, connStatus = s, statusMsg = "Connected — loading schemas…"), Cmd.LoadSchemas)

    case Msg.SchemasLoaded(schemas) =>
      (model.copy(schemas = schemas, cursor = 0, statusMsg = s"${schemas.size} schema(s) loaded"), Cmd.None)

    case Msg.TablesLoaded(tables) =>
      (model.copy(tables = tables, cursor = 0, activeView = View.SchemaBrowser, statusMsg = s"${tables.size} table(s)"), Cmd.None)

    case Msg.PrimaryKeysLoaded(pkCols) =>
      (model.copy(pkColumns = pkCols), Cmd.None)

    case Msg.DataLoaded(cols, rows) =>
      // Trigger PK load after data arrives so we know which column to use for UPDATE.
      val pkCmd = (model.selectedSchema, model.selectedTable) match
        case (Some(s), Some(t)) => Cmd.LoadPrimaryKeys(s, t)
        case _                  => Cmd.None
      (model.copy(columns = cols, rows = rows, selectedRow = 0, selectedCol = 0, activeView = View.DataViewer, statusMsg = s"${rows.size} row(s)"), pkCmd)

    case Msg.QueryResult(cols, rows) =>
      (model.copy(queryColumns = cols, queryRows = rows, statusMsg = s"Query returned ${rows.size} row(s)"), Cmd.None)

    case Msg.CellUpdated(n) =>
      val newModel = model.copy(editState = EditState.Idle, statusMsg = s"Updated $n row(s)")
      val cmd = (model.selectedSchema, model.selectedTable) match
        case (Some(s), Some(t)) => Cmd.LoadData(s, t, model.dataLimit, model.dataOffset)
        case _                  => Cmd.None
      (newModel, cmd)

    case Msg.DbError(err) =>
      (model.copy(statusMsg = s"Error: $err"), Cmd.None)

    case Msg.MoveUp =>
      model.activeView match
        case View.SchemaBrowser =>
          val newCursor = (model.cursor - 1).max(0)
          (model.copy(cursor = newCursor), Cmd.None)
        case View.DataViewer =>
          val newRow = (model.selectedRow - 1).max(0)
          (model.copy(selectedRow = newRow), Cmd.None)
        case View.QueryConsole =>
          (model, Cmd.None)

    case Msg.MoveDown =>
      model.activeView match
        case View.SchemaBrowser =>
          val items     = if model.selectedSchema.isDefined then model.tables else model.schemas.map(_.name)
          val newCursor = (model.cursor + 1).min((items.size - 1).max(0))
          (model.copy(cursor = newCursor), Cmd.None)
        case View.DataViewer =>
          val newRow = (model.selectedRow + 1).min((model.rows.size - 1).max(0))
          (model.copy(selectedRow = newRow), Cmd.None)
        case View.QueryConsole =>
          (model, Cmd.None)

    case Msg.MoveLeft =>
      model.activeView match
        case View.DataViewer =>
          val newCol = (model.selectedCol - 1).max(0)
          (model.copy(selectedCol = newCol), Cmd.None)
        case _ =>
          (model, Cmd.None)

    case Msg.MoveRight =>
      model.activeView match
        case View.DataViewer =>
          val newCol = (model.selectedCol + 1).min((model.columns.size - 1).max(0))
          (model.copy(selectedCol = newCol), Cmd.None)
        case _ =>
          (model, Cmd.None)

    case Msg.Select =>
      model.activeView match
        case View.SchemaBrowser =>
          if model.selectedSchema.isDefined then
            // select table
            val table = model.tables.lift(model.cursor)
            table match
              case Some(t) =>
                val schema = model.selectedSchema.get
                // Load data; PK columns are loaded separately as a second command.
                // We chain by emitting LoadData here and LoadPrimaryKeys is dispatched
                // after DataLoaded arrives (see Msg.DataLoaded handler).
                (model.copy(selectedTable = Some(t), pkColumns = Nil, statusMsg = s"Loading $t…"), Cmd.LoadData(schema, t, model.dataLimit, model.dataOffset))
              case None =>
                (model, Cmd.None)
          else
            // expand schema
            val schema = model.schemas.lift(model.cursor).map(_.name)
            schema match
              case Some(s) =>
                (model.copy(selectedSchema = Some(s), statusMsg = s"Loading tables for $s…"), Cmd.LoadTables(s))
              case None =>
                (model, Cmd.None)
        case _ =>
          (model, Cmd.None)

    case Msg.Back =>
      model.activeView match
        case View.DataViewer =>
          (model.copy(activeView = View.SchemaBrowser, statusMsg = ""), Cmd.None)
        case View.QueryConsole =>
          (model.copy(activeView = View.SchemaBrowser, statusMsg = ""), Cmd.None)
        case View.SchemaBrowser =>
          if model.selectedSchema.isDefined then
            (model.copy(selectedSchema = None, selectedTable = None, tables = Nil, cursor = 0, statusMsg = ""), Cmd.None)
          else
            (model, Cmd.None)

    case Msg.SwitchView(v) =>
      (model.copy(activeView = v, statusMsg = ""), Cmd.None)

    // ── Search ────────────────────────────────────────────────────
    case Msg.StartSearch =>
      (model.copy(searching = true, searchInput = "", statusMsg = "Search: "), Cmd.None)

    case Msg.SearchInput(c) =>
      val s = model.searchInput + c
      (model.copy(searchInput = s, statusMsg = s"Search: $s"), Cmd.None)

    case Msg.SearchBackspace =>
      val s = if model.searchInput.nonEmpty then model.searchInput.init else ""
      (model.copy(searchInput = s, statusMsg = s"Search: $s"), Cmd.None)

    case Msg.SearchConfirm =>
      (model.copy(searching = false, statusMsg = s"Filtered by: ${model.searchInput}"), Cmd.None)

    case Msg.SearchCancel =>
      (model.copy(searching = false, searchInput = "", statusMsg = ""), Cmd.None)

    // ── Edit ──────────────────────────────────────────────────────
    case Msg.StartEdit =>
      model.activeView match
        case View.DataViewer =>
          val currentValue = model.rows
            .lift(model.selectedRow)
            .flatMap(r => model.columns.lift(model.selectedCol).flatMap(c => r.cells.get(c)))
            .getOrElse("")
          (model.copy(editState = EditState.EditingCell(currentValue), statusMsg = s"Editing: $currentValue"), Cmd.None)
        case _ =>
          (model, Cmd.None)

    case Msg.EditInput(c) =>
      model.editState match
        case EditState.EditingCell(v) =>
          val newVal = v + c
          (model.copy(editState = EditState.EditingCell(newVal), statusMsg = s"Editing: $newVal"), Cmd.None)
        case _ =>
          (model, Cmd.None)

    case Msg.EditBackspace =>
      model.editState match
        case EditState.EditingCell(v) =>
          val newVal = if v.nonEmpty then v.init else ""
          (model.copy(editState = EditState.EditingCell(newVal), statusMsg = s"Editing: $newVal"), Cmd.None)
        case _ =>
          (model, Cmd.None)

    case Msg.CommitEdit =>
      model.editState match
        case EditState.EditingCell(v) =>
          val schema = model.selectedSchema.getOrElse("")
          val table  = model.selectedTable.getOrElse("")
          val col    = model.columns.lift(model.selectedCol).getOrElse("")
          // Build composite PK conditions from all PK columns queried from INFORMATION_SCHEMA.
          val pkConditions: Map[String, String] = model.pkColumns.flatMap { pkCol =>
            model.rows.lift(model.selectedRow).flatMap(_.cells.get(pkCol)).map(pkCol -> _)
          }.toMap
          if schema.isEmpty || table.isEmpty || col.isEmpty then
            (model.copy(editState = EditState.Idle, statusMsg = "Cannot edit: no column selected"), Cmd.None)
          else if model.pkColumns.isEmpty then
            (model.copy(editState = EditState.Idle, statusMsg = "Cannot edit: no primary key found for this table"), Cmd.None)
          else if pkConditions.size < model.pkColumns.size then
            (model.copy(editState = EditState.Idle, statusMsg = "Cannot edit: PK value(s) missing from current row"), Cmd.None)
          else
            (model.copy(statusMsg = s"Saving…"), Cmd.UpdateCell(schema, table, pkConditions, col, v))
        case _ =>
          (model, Cmd.None)

    case Msg.CancelEdit =>
      (model.copy(editState = EditState.Idle, statusMsg = "Edit cancelled"), Cmd.None)

    // ── Query Console ─────────────────────────────────────────────
    case Msg.QueryInput(c) =>
      val q = model.queryInput + c
      (model.copy(queryInput = q), Cmd.None)

    case Msg.QueryBackspace =>
      val q = if model.queryInput.nonEmpty then model.queryInput.init else ""
      (model.copy(queryInput = q), Cmd.None)

    case Msg.ExecuteQuery =>
      if model.queryInput.trim.nonEmpty then
        (model.copy(statusMsg = "Executing query…"), Cmd.RunQuery(model.queryInput.trim))
      else
        (model, Cmd.None)

    // ── Context-aware raw input routing ──────────────────────────
    case Msg.CharInput(c) =>
      if model.searching then
        apply(model, Msg.SearchInput(c))
      else model.activeView match
        case View.QueryConsole =>
          apply(model, Msg.QueryInput(c))
        case View.DataViewer =>
          model.editState match
            case EditState.EditingCell(_) => apply(model, Msg.EditInput(c))
            case _                        => (model, Cmd.None)
        case _ =>
          (model, Cmd.None)

    case Msg.BackspaceInput =>
      if model.searching then
        apply(model, Msg.SearchBackspace)
      else model.activeView match
        case View.QueryConsole =>
          apply(model, Msg.QueryBackspace)
        case View.DataViewer =>
          model.editState match
            case EditState.EditingCell(_) => apply(model, Msg.EditBackspace)
            case _                        => (model, Cmd.None)
        case _ =>
          (model, Cmd.None)

    case Msg.ConfirmInput =>
      if model.searching then
        apply(model, Msg.SearchConfirm)
      else model.activeView match
        case View.QueryConsole => apply(model, Msg.ExecuteQuery)
        case View.DataViewer =>
          model.editState match
            case EditState.EditingCell(_) => apply(model, Msg.CommitEdit)
            case _                        => apply(model, Msg.Select)
        case _ => apply(model, Msg.Select)

    case Msg.Quit =>
      (model, Cmd.None)

// ─────────────────────────────────────────────────────────────────
//  View — pure rendering to a String
// ─────────────────────────────────────────────────────────────────
object Renderer:

  private val ColWidth    = 18
  private val MaxVisible  = 20
  private val BoxH        = "─"
  private val BoxV        = "│"
  private val BoxTL       = "╭"
  private val BoxTR       = "╮"
  private val BoxBL       = "╰"
  private val BoxBR       = "╯"
  private val BoxT        = "┬"
  private val BoxB        = "┴"
  private val BoxMid      = "┼"
  private val BoxML       = "├"
  private val BoxMR       = "┤"
  private val BoxMH       = "─"

  def render(model: AppModel): String =
    val sb = new StringBuilder
    sb.append(Ansi.ClearScreen)
    sb.append(renderHeader(model))
    sb.append("\n")
    model.activeView match
      case View.SchemaBrowser => sb.append(renderSchemaBrowser(model))
      case View.DataViewer    => sb.append(renderDataViewer(model))
      case View.QueryConsole  => sb.append(renderQueryConsole(model))
    sb.append("\n")
    sb.append(renderFooter(model))
    sb.toString

  // ── Header ───────────────────────────────────────────────────
  private def renderHeader(model: AppModel): String =
    val connColor = if model.connected then Ansi.Green else Ansi.Red
    val connIcon  = if model.connected then "✔" else "✘"
    val schemaStr = model.selectedSchema.map(s => s" 🗄️  $s").getOrElse("")
    val tableStr  = model.selectedTable.map(t  => s" 📋 $t").getOrElse("")
    val title     = Ansi.bold(Ansi.colored(Ansi.Cyan, " ⬡  Database Viewer"))
    val conn      = Ansi.colored(connColor, s" $connIcon ${model.connStatus}")
    val path      = Ansi.colored(Ansi.Yellow, s"$schemaStr$tableStr")
    s"$BoxTL${BoxH * 78}$BoxTR\n$BoxV $title$conn$path\n$BoxBL${BoxH * 78}$BoxBR"

  // ── Schema Browser ───────────────────────────────────────────
  private def renderSchemaBrowser(model: AppModel): String =
    val sb = new StringBuilder
    val items: List[String] =
      if model.selectedSchema.isDefined then
        // show tables, filtered by search
        val filter = model.searchInput.toLowerCase
        model.tables.filter(t => filter.isEmpty || t.toLowerCase.contains(filter))
      else
        val filter = model.searchInput.toLowerCase
        model.schemas
          .filter(s => filter.isEmpty || s.name.toLowerCase.contains(filter))
          .map(s => s"${s.name}  (${s.tableCount} tables)")

    val title = if model.selectedSchema.isDefined then
      s"  ${Ansi.colored(Ansi.Cyan, "Tables")} in ${Ansi.colored(Ansi.Yellow, model.selectedSchema.get)}"
    else
      s"  ${Ansi.colored(Ansi.Cyan, "Schemas")}"

    sb.append(title + "\n")
    sb.append(s"  $BoxTL${BoxH * 48}$BoxTR\n")
    val visible = items.drop((model.cursor - MaxVisible / 2).max(0)).take(MaxVisible)
    val startIdx = (model.cursor - MaxVisible / 2).max(0)
    if visible.isEmpty then
      sb.append(s"  $BoxV  ${Ansi.colored(Ansi.Gray, "(empty)")}${" " * 41}$BoxV\n")
    else
      visible.zipWithIndex.foreach { case (item, i) =>
        val realIdx = startIdx + i
        val isSel   = realIdx == model.cursor
        val prefix  = if isSel then s"${Ansi.Blue}▶ ${Ansi.Reset}" else "  "
        val content = item.take(44).padTo(44, ' ')
        val line    = if isSel then Ansi.colored(Ansi.White, content) else Ansi.colored(Ansi.Gray, content)
        sb.append(s"  $BoxV $prefix$line$BoxV\n")
      }
    sb.append(s"  $BoxBL${BoxH * 48}$BoxBR\n")
    sb.toString

  // ── Data Viewer ──────────────────────────────────────────────
  private def renderDataViewer(model: AppModel): String =
    val sb = new StringBuilder
    val schema = model.selectedSchema.getOrElse("?")
    val table  = model.selectedTable.getOrElse("?")
    sb.append(s"  ${Ansi.colored(Ansi.Cyan, "Data")} from ${Ansi.colored(Ansi.Yellow, s"$schema.$table")} (offset ${model.dataOffset})\n\n")

    if model.columns.isEmpty then
      sb.append(s"  ${Ansi.colored(Ansi.Gray, "(no data)")}\n")
    else
      val visibleCols = model.columns.take(6)
      val colW        = ColWidth

      // header row
      val headerCells = visibleCols.map(c => Ansi.bold(c.take(colW - 2).padTo(colW - 2, ' ')))
      sb.append("  " + headerCells.mkString(s" $BoxV ") + "\n")
      sb.append("  " + (BoxMH * ((colW) * visibleCols.size + visibleCols.size - 1)) + "\n")

      // data rows
      val visibleRows = model.rows.drop((model.selectedRow - MaxVisible / 2).max(0)).take(MaxVisible)
      val startRow    = (model.selectedRow - MaxVisible / 2).max(0)
      visibleRows.zipWithIndex.foreach { case (row, i) =>
        val realRow = startRow + i
        val isSelRow = realRow == model.selectedRow
        val cells = visibleCols.zipWithIndex.map { case (col, ci) =>
          val v       = row.cells.getOrElse(col, "")
          val clipped = v.take(colW - 2).padTo(colW - 2, ' ')
          model.editState match
            case EditState.EditingCell(ev) if isSelRow && ci == model.selectedCol =>
              Ansi.colored(Ansi.Magenta, s"[$ev]".take(colW - 2).padTo(colW - 2, ' '))
            case _ if isSelRow && ci == model.selectedCol =>
              Ansi.colored(Ansi.Cyan, clipped)
            case _ if isSelRow =>
              Ansi.colored(Ansi.Blue, clipped)
            case _ =>
              Ansi.colored(Ansi.Gray, clipped)
        }
        sb.append("  " + cells.mkString(s" $BoxV ") + "\n")
      }

    sb.toString

  // ── Query Console ────────────────────────────────────────────
  private def renderQueryConsole(model: AppModel): String =
    val sb = new StringBuilder
    sb.append(s"  ${Ansi.colored(Ansi.Cyan, "SQL Query Console")}\n\n")
    sb.append(s"  ${Ansi.bold("SQL>")} ${model.queryInput}${Ansi.colored(Ansi.Cyan, "█")}\n\n")

    if model.queryColumns.nonEmpty then
      val cols    = model.queryColumns.take(6)
      val colW    = ColWidth
      val header  = cols.map(c => Ansi.bold(c.take(colW - 2).padTo(colW - 2, ' '))).mkString(s" $BoxV ")
      sb.append(s"  $header\n")
      sb.append("  " + (BoxMH * (colW * cols.size + cols.size - 1)) + "\n")
      model.queryRows.take(MaxVisible).foreach { row =>
        val cells = cols.map { col =>
          val v = row.cells.getOrElse(col, "")
          Ansi.colored(Ansi.Gray, v.take(colW - 2).padTo(colW - 2, ' '))
        }
        sb.append("  " + cells.mkString(s" $BoxV ") + "\n")
      }

    sb.toString

  // ── Footer ───────────────────────────────────────────────────
  private def renderFooter(model: AppModel): String =
    val keys = model.activeView match
      case View.SchemaBrowser =>
        if model.searching then s"${Ansi.bold("<Enter>")} confirm  ${Ansi.bold("<Esc>")} cancel"
        else s"${Ansi.bold("j/k")} navigate  ${Ansi.bold("Enter")} select  ${Ansi.bold("/")} search  ${Ansi.bold("c")} query console  ${Ansi.bold("q")} back/quit"
      case View.DataViewer =>
        model.editState match
          case EditState.EditingCell(_) => s"${Ansi.bold("Ctrl+S")} commit  ${Ansi.bold("Esc")} cancel"
          case _ => s"${Ansi.bold("h/j/k/l")} navigate  ${Ansi.bold("e")} edit  ${Ansi.bold("q")} back"
      case View.QueryConsole =>
        s"${Ansi.bold("Enter")} execute  ${Ansi.bold("Backspace")} delete  ${Ansi.bold("q")} back"

    val status = Ansi.colored(Ansi.Yellow, model.statusMsg)
    s"$BoxML${BoxH * 78}$BoxMR\n$BoxV  $keys\n$BoxV  $status\n$BoxBL${BoxH * 78}$BoxBR"
