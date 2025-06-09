package grammar.terminal

enum Position {
  case InBend
  case InChevron
  case InFess
  case InPale
  case InPile
  case InCross
  case InSaltire
}

object Position {
  def repr(d: Position): Set[String] = d match {
    case InBend => Set("in bend")
    case InChevron => Set("in chev.", "in chevron")
    case InFess => Set("in fess")
    case InPale => Set("in pale")
    case InPile => Set("in pile")
    case InCross => Set("in cross")
    case InSaltire => Set("in saltire")
  }

  val reprs: Map[Position, Set[String]] = Position.values.map(c => (c, repr(c))).toMap

  def canDisplay(p: Position, n: Int): Boolean = p match {
    case InBend => n >= 2
    case InChevron => n >= 3
    case InFess => n >= 2
    case InPale => n >= 2
    case InPile => n >= 3
    case InCross => n >= 4 && (n % 4 == 0 || n % 4 == 1)
    case InSaltire => n >= 4 && (n % 4 == 0 || n % 4 == 1)
  }
}
