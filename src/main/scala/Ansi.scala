// ANSI terminal escape code helpers.
object Ansi:
  val Reset        = "\u001b[0m"
  val Bold         = "\u001b[1m"
  val Dim          = "\u001b[2m"
  val ClearScreen  = "\u001b[2J\u001b[H"
  val HideCursor   = "\u001b[?25l"
  val ShowCursor   = "\u001b[?25h"

  // Foreground colours (256-colour)
  def fg(n: Int) = s"\u001b[38;5;${n}m"
  def bg(n: Int) = s"\u001b[48;5;${n}m"

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
