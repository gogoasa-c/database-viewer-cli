class UpdateSpec extends munit.FunSuite:

  // ── Helpers ──────────────────────────────────────────────────────
  private val base = AppModel()

  private def update(model: AppModel, msg: Msg): (AppModel, Cmd) = Update(model, msg)

  // ── Lifecycle ────────────────────────────────────────────────────
  test("Init emits Ping") {
    val (_, cmd) = update(base, Msg.Init)
    assertEquals(cmd, Cmd.Ping)
  }

  test("Connected sets connected flag, triggers LoadSchemas") {
    val (model, cmd) = update(base, Msg.Connected("localhost:3306"))
    assert(model.connected)
    assertEquals(model.connStatus, "localhost:3306")
    assertEquals(cmd, Cmd.LoadSchemas)
  }

  test("SchemasLoaded stores schemas and resets cursor") {
    val schemas = List(SchemaInfo("db1", 3), SchemaInfo("db2", 1))
    val (model, cmd) = update(base.copy(cursor = 5), Msg.SchemasLoaded(schemas))
    assertEquals(model.schemas, schemas)
    assertEquals(model.cursor, 0)
    assertEquals(cmd, Cmd.None)
  }

  test("TablesLoaded stores tables, resets cursor, stays in SchemaBrowser") {
    val (model, cmd) = update(base, Msg.TablesLoaded(List("users", "orders")))
    assertEquals(model.tables, List("users", "orders"))
    assertEquals(model.cursor, 0)
    assertEquals(model.activeView, View.SchemaBrowser)
    assertEquals(cmd, Cmd.None)
  }

  test("PrimaryKeysLoaded stores pk columns") {
    val (model, cmd) = update(base, Msg.PrimaryKeysLoaded(List("id", "tenant_id")))
    assertEquals(model.pkColumns, List("id", "tenant_id"))
    assertEquals(cmd, Cmd.None)
  }

  test("DataLoaded switches to DataViewer, issues LoadPrimaryKeys when schema+table known") {
    val m = base.copy(selectedSchema = Some("mydb"), selectedTable = Some("users"))
    val cols = List("id", "name")
    val rows = List(Row(Map("id" -> "1", "name" -> "Alice")))
    val (model, cmd) = update(m, Msg.DataLoaded(cols, rows))
    assertEquals(model.activeView, View.DataViewer)
    assertEquals(model.columns, cols)
    assertEquals(model.rows, rows)
    assertEquals(model.selectedRow, 0)
    assertEquals(model.selectedCol, 0)
    assertEquals(cmd, Cmd.LoadPrimaryKeys("mydb", "users"))
  }

  test("DataLoaded emits Cmd.None when schema/table missing") {
    val (_, cmd) = update(base, Msg.DataLoaded(List("id"), List()))
    assertEquals(cmd, Cmd.None)
  }

  test("QueryResult stores query results") {
    val cols = List("count")
    val rows = List(Row(Map("count" -> "42")))
    val (model, cmd) = update(base, Msg.QueryResult(cols, rows))
    assertEquals(model.queryColumns, cols)
    assertEquals(model.queryRows, rows)
    assertEquals(cmd, Cmd.None)
  }

  test("CellUpdated resets editState, reloads data if schema+table known") {
    val m = base.copy(
      selectedSchema = Some("mydb"),
      selectedTable  = Some("users"),
      editState      = EditState.EditingCell("foo"),
    )
    val (model, cmd) = update(m, Msg.CellUpdated(1))
    assertEquals(model.editState, EditState.Idle)
    assertEquals(cmd, Cmd.LoadData("mydb", "users", base.dataLimit, base.dataOffset))
  }

  test("DbError stores error message") {
    val (model, cmd) = update(base, Msg.DbError("connection refused"))
    assert(model.statusMsg.contains("connection refused"))
    assertEquals(cmd, Cmd.None)
  }

  test("Quit returns Cmd.None") {
    val (_, cmd) = update(base, Msg.Quit)
    assertEquals(cmd, Cmd.None)
  }

  // ── Navigation — SchemaBrowser ───────────────────────────────────
  test("MoveUp clamps cursor to 0 in SchemaBrowser") {
    val m = base.copy(cursor = 0)
    val (model, _) = update(m, Msg.MoveUp)
    assertEquals(model.cursor, 0)
  }

  test("MoveDown advances cursor within schema list bounds") {
    val schemas = List(SchemaInfo("a", 1), SchemaInfo("b", 2), SchemaInfo("c", 3))
    val m = base.copy(schemas = schemas, cursor = 1)
    val (model, _) = update(m, Msg.MoveDown)
    assertEquals(model.cursor, 2)
  }

  test("MoveDown clamps cursor at last index in SchemaBrowser") {
    val schemas = List(SchemaInfo("a", 1), SchemaInfo("b", 2))
    val m = base.copy(schemas = schemas, cursor = 1)
    val (model, _) = update(m, Msg.MoveDown)
    assertEquals(model.cursor, 1)
  }

  test("MoveDown on empty list keeps cursor at 0") {
    val (model, _) = update(base, Msg.MoveDown)
    assertEquals(model.cursor, 0)
  }

  // ── Navigation — DataViewer ──────────────────────────────────────
  test("MoveUp in DataViewer decrements selectedRow (clamp at 0)") {
    val m = base.copy(activeView = View.DataViewer, selectedRow = 0)
    val (model, _) = update(m, Msg.MoveUp)
    assertEquals(model.selectedRow, 0)
  }

  test("MoveDown in DataViewer increments selectedRow") {
    val rows = List(Row(Map("id" -> "1")), Row(Map("id" -> "2")))
    val m = base.copy(activeView = View.DataViewer, rows = rows, selectedRow = 0)
    val (model, _) = update(m, Msg.MoveDown)
    assertEquals(model.selectedRow, 1)
  }

  test("MoveLeft in DataViewer decrements selectedCol (clamp at 0)") {
    val m = base.copy(activeView = View.DataViewer, selectedCol = 0)
    val (model, _) = update(m, Msg.MoveLeft)
    assertEquals(model.selectedCol, 0)
  }

  test("MoveRight in DataViewer increments selectedCol") {
    val m = base.copy(activeView = View.DataViewer, columns = List("a", "b", "c"), selectedCol = 0)
    val (model, _) = update(m, Msg.MoveRight)
    assertEquals(model.selectedCol, 1)
  }

  test("MoveRight in DataViewer clamps at last column") {
    val m = base.copy(activeView = View.DataViewer, columns = List("a", "b"), selectedCol = 1)
    val (model, _) = update(m, Msg.MoveRight)
    assertEquals(model.selectedCol, 1)
  }

  test("MoveLeft/Right are no-ops outside DataViewer") {
    val m = base.copy(activeView = View.SchemaBrowser, selectedCol = 2)
    val (ml, _) = update(m, Msg.MoveLeft)
    val (mr, _) = update(m, Msg.MoveRight)
    assertEquals(ml.selectedCol, 2)
    assertEquals(mr.selectedCol, 2)
  }

  // ── Select ───────────────────────────────────────────────────────
  test("Select expands schema when none selected") {
    val schemas = List(SchemaInfo("mydb", 3))
    val m = base.copy(schemas = schemas, cursor = 0)
    val (model, cmd) = update(m, Msg.Select)
    assertEquals(model.selectedSchema, Some("mydb"))
    assertEquals(cmd, Cmd.LoadTables("mydb"))
  }

  test("Select loads table data when schema already selected") {
    val m = base.copy(
      selectedSchema = Some("mydb"),
      tables         = List("users", "orders"),
      cursor         = 1,
    )
    val (model, cmd) = update(m, Msg.Select)
    assertEquals(model.selectedTable, Some("orders"))
    assertEquals(cmd, Cmd.LoadData("mydb", "orders", base.dataLimit, base.dataOffset))
  }

  test("Select on empty schema list is a no-op") {
    val (model, cmd) = update(base, Msg.Select)
    assertEquals(model, base)
    assertEquals(cmd, Cmd.None)
  }

  // ── Back ─────────────────────────────────────────────────────────
  test("Back from DataViewer returns to SchemaBrowser") {
    val m = base.copy(activeView = View.DataViewer)
    val (model, cmd) = update(m, Msg.Back)
    assertEquals(model.activeView, View.SchemaBrowser)
    assertEquals(cmd, Cmd.None)
  }

  test("Back from QueryConsole returns to SchemaBrowser") {
    val m = base.copy(activeView = View.QueryConsole)
    val (model, cmd) = update(m, Msg.Back)
    assertEquals(model.activeView, View.SchemaBrowser)
    assertEquals(cmd, Cmd.None)
  }

  test("Back in SchemaBrowser with schema selected clears schema") {
    val m = base.copy(selectedSchema = Some("mydb"), tables = List("t1"))
    val (model, _) = update(m, Msg.Back)
    assertEquals(model.selectedSchema, None)
    assertEquals(model.tables, Nil)
    assertEquals(model.cursor, 0)
  }

  test("Back in SchemaBrowser with no schema is a no-op") {
    val (model, cmd) = update(base, Msg.Back)
    assertEquals(model, base)
    assertEquals(cmd, Cmd.None)
  }

  // ── SwitchView ───────────────────────────────────────────────────
  test("SwitchView changes activeView") {
    val (model, _) = update(base, Msg.SwitchView(View.QueryConsole))
    assertEquals(model.activeView, View.QueryConsole)
  }

  // ── Search ───────────────────────────────────────────────────────
  test("StartSearch enters searching mode") {
    val (model, cmd) = update(base, Msg.StartSearch)
    assert(model.searching)
    assertEquals(model.searchInput, "")
    assertEquals(cmd, Cmd.None)
  }

  test("SearchInput appends character") {
    val m = base.copy(searching = true, searchInput = "fo")
    val (model, _) = update(m, Msg.SearchInput('o'))
    assertEquals(model.searchInput, "foo")
  }

  test("SearchBackspace removes last character") {
    val m = base.copy(searching = true, searchInput = "foo")
    val (model, _) = update(m, Msg.SearchBackspace)
    assertEquals(model.searchInput, "fo")
  }

  test("SearchBackspace on empty string stays empty") {
    val m = base.copy(searching = true, searchInput = "")
    val (model, _) = update(m, Msg.SearchBackspace)
    assertEquals(model.searchInput, "")
  }

  test("SearchConfirm exits searching mode") {
    val m = base.copy(searching = true, searchInput = "foo")
    val (model, _) = update(m, Msg.SearchConfirm)
    assert(!model.searching)
  }

  test("SearchCancel exits searching mode and clears input") {
    val m = base.copy(searching = true, searchInput = "foo")
    val (model, _) = update(m, Msg.SearchCancel)
    assert(!model.searching)
    assertEquals(model.searchInput, "")
  }

  // ── Edit ─────────────────────────────────────────────────────────
  test("StartEdit in DataViewer enters EditingCell with current value") {
    val row = Row(Map("name" -> "Alice"))
    val m = base.copy(
      activeView  = View.DataViewer,
      columns     = List("name"),
      rows        = List(row),
      selectedRow = 0,
      selectedCol = 0,
    )
    val (model, _) = update(m, Msg.StartEdit)
    assertEquals(model.editState, EditState.EditingCell("Alice"))
  }

  test("StartEdit outside DataViewer is a no-op") {
    val (model, _) = update(base, Msg.StartEdit)
    assertEquals(model.editState, EditState.Idle)
  }

  test("EditInput appends character to current edit value") {
    val m = base.copy(editState = EditState.EditingCell("foo"))
    val (model, _) = update(m, Msg.EditInput('!'))
    assertEquals(model.editState, EditState.EditingCell("foo!"))
  }

  test("EditBackspace removes last character from edit value") {
    val m = base.copy(editState = EditState.EditingCell("foo"))
    val (model, _) = update(m, Msg.EditBackspace)
    assertEquals(model.editState, EditState.EditingCell("fo"))
  }

  test("EditBackspace on empty edit value stays empty") {
    val m = base.copy(editState = EditState.EditingCell(""))
    val (model, _) = update(m, Msg.EditBackspace)
    assertEquals(model.editState, EditState.EditingCell(""))
  }

  test("CancelEdit resets editState to Idle") {
    val m = base.copy(editState = EditState.EditingCell("foo"))
    val (model, cmd) = update(m, Msg.CancelEdit)
    assertEquals(model.editState, EditState.Idle)
    assertEquals(cmd, Cmd.None)
  }

  test("CommitEdit emits UpdateCell with composite PK conditions") {
    val row = Row(Map("id" -> "42", "tenant" -> "acme", "name" -> "Alice"))
    val m = base.copy(
      selectedSchema = Some("mydb"),
      selectedTable  = Some("users"),
      columns        = List("id", "tenant", "name"),
      pkColumns      = List("id", "tenant"),
      rows           = List(row),
      selectedRow    = 0,
      selectedCol    = 2,
      editState      = EditState.EditingCell("Bob"),
    )
    val (model, cmd) = update(m, Msg.CommitEdit)
    assertEquals(cmd, Cmd.UpdateCell("mydb", "users", Map("id" -> "42", "tenant" -> "acme"), "name", "Bob"))
    assertEquals(model.statusMsg, "Saving…")
  }

  test("CommitEdit without pkColumns rejects edit") {
    val m = base.copy(
      selectedSchema = Some("mydb"),
      selectedTable  = Some("users"),
      columns        = List("name"),
      pkColumns      = Nil,
      rows           = List(Row(Map("name" -> "Alice"))),
      selectedRow    = 0,
      selectedCol    = 0,
      editState      = EditState.EditingCell("Bob"),
    )
    val (model, cmd) = update(m, Msg.CommitEdit)
    assertEquals(cmd, Cmd.None)
    assertEquals(model.editState, EditState.Idle)
    assert(model.statusMsg.contains("no primary key"))
  }

  // ── Query Console ────────────────────────────────────────────────
  test("QueryInput appends character to queryInput") {
    val m = base.copy(queryInput = "SELECT ")
    val (model, _) = update(m, Msg.QueryInput('1'))
    assertEquals(model.queryInput, "SELECT 1")
  }

  test("QueryBackspace removes last character") {
    val m = base.copy(queryInput = "SELECT 1")
    val (model, _) = update(m, Msg.QueryBackspace)
    assertEquals(model.queryInput, "SELECT ")
  }

  test("ExecuteQuery emits RunQuery for non-blank input") {
    val m = base.copy(queryInput = "SELECT 1")
    val (_, cmd) = update(m, Msg.ExecuteQuery)
    assertEquals(cmd, Cmd.RunQuery("SELECT 1"))
  }

  test("ExecuteQuery is a no-op for blank queryInput") {
    val (_, cmd) = update(base.copy(queryInput = "   "), Msg.ExecuteQuery)
    assertEquals(cmd, Cmd.None)
  }

  // ── Context-aware input routing ──────────────────────────────────
  test("CharInput routes to SearchInput when searching") {
    val m = base.copy(searching = true, searchInput = "")
    val (model, _) = update(m, Msg.CharInput('x'))
    assertEquals(model.searchInput, "x")
  }

  test("CharInput routes to QueryInput in QueryConsole") {
    val m = base.copy(activeView = View.QueryConsole, queryInput = "")
    val (model, _) = update(m, Msg.CharInput('S'))
    assertEquals(model.queryInput, "S")
  }

  test("CharInput routes to EditInput when editing a cell") {
    val m = base.copy(activeView = View.DataViewer, editState = EditState.EditingCell("hi"))
    val (model, _) = update(m, Msg.CharInput('!'))
    assertEquals(model.editState, EditState.EditingCell("hi!"))
  }

  test("BackspaceInput routes to SearchBackspace when searching") {
    val m = base.copy(searching = true, searchInput = "foo")
    val (model, _) = update(m, Msg.BackspaceInput)
    assertEquals(model.searchInput, "fo")
  }

  test("BackspaceInput routes to QueryBackspace in QueryConsole") {
    val m = base.copy(activeView = View.QueryConsole, queryInput = "SELECT 1")
    val (model, _) = update(m, Msg.BackspaceInput)
    assertEquals(model.queryInput, "SELECT ")
  }

  test("BackspaceInput routes to EditBackspace when editing") {
    val m = base.copy(activeView = View.DataViewer, editState = EditState.EditingCell("foo"))
    val (model, _) = update(m, Msg.BackspaceInput)
    assertEquals(model.editState, EditState.EditingCell("fo"))
  }

  test("ConfirmInput in QueryConsole executes query") {
    val m = base.copy(activeView = View.QueryConsole, queryInput = "SELECT 1")
    val (_, cmd) = update(m, Msg.ConfirmInput)
    assertEquals(cmd, Cmd.RunQuery("SELECT 1"))
  }

  test("ConfirmInput while searching confirms search") {
    val m = base.copy(searching = true, searchInput = "foo")
    val (model, _) = update(m, Msg.ConfirmInput)
    assert(!model.searching)
  }

  test("ConfirmInput while editing a cell commits edit") {
    val row = Row(Map("id" -> "1", "name" -> "Alice"))
    val m = base.copy(
      activeView     = View.DataViewer,
      selectedSchema = Some("mydb"),
      selectedTable  = Some("users"),
      columns        = List("id", "name"),
      pkColumns      = List("id"),
      rows           = List(row),
      selectedRow    = 0,
      selectedCol    = 1,
      editState      = EditState.EditingCell("Bob"),
    )
    val (_, cmd) = update(m, Msg.ConfirmInput)
    assertEquals(cmd, Cmd.UpdateCell("mydb", "users", Map("id" -> "1"), "name", "Bob"))
  }
