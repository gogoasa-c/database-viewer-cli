// Update — pure MVU function: (AppModel, Msg) => (AppModel, Cmd)
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
