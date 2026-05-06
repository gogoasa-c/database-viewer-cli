// Message and Command ADTs for the MVU loop.

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
