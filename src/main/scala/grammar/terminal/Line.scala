package grammar.terminal

enum Line {
  case Dancetty
  case Embattled
  case Indented
  case Nebuly
  case Wavy
  case Engrailed
  case Raguly
}

object Line {
  def repr(l: Line): Set[String] = l match {
    case Dancetty => Set("dancetty")
    case Embattled => Set("embattled")
    case Indented => Set("indented")
    case Nebuly => Set("nebuly")
    case Wavy => Set("wavy")
    case Engrailed => Set("engrailed", "engr.")
    case Raguly => Set("raguly")
  }

  val reprs: Map[Line, Set[String]] = Line.values.map(c => (c, repr(c))).toMap
}