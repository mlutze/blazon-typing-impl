package grammar.terminal

enum Ordinary {
  case Bend
  case Chevron
  case Chief
  case Cross
  case Fess
  case Pale
  case Saltire
  case Bendlet
}

object Ordinary {
  def repr(o: Ordinary): Set[String] = o match {
    case Bend => Set("bend")
    case Chevron => Set("chevron", "chev.", "chev")
    case Chief => Set("chief")
    case Cross => Set("cross")
    case Fess => Set("fess")
    case Pale => Set("pale")
    case Saltire => Set("saltire")
    case Bendlet => Set("bendlet")
  }

  val reprs: Map[Ordinary, Set[String]] = Ordinary.values.map(c => (c, repr(c))).toMap
}