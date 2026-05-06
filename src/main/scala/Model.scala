// Domain model: view states, edit states, and the application model.

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
  pkColumns:       List[String]        = Nil,
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
