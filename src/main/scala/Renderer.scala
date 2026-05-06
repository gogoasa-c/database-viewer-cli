// Renderer — pure function: AppModel => ANSI-rendered String
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
    val visible  = items.drop((model.cursor - MaxVisible / 2).max(0)).take(MaxVisible)
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
        val realRow  = startRow + i
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
      val cols   = model.queryColumns.take(6)
      val colW   = ColWidth
      val header = cols.map(c => Ansi.bold(c.take(colW - 2).padTo(colW - 2, ' '))).mkString(s" $BoxV ")
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
