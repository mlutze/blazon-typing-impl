package grammar.terminal

enum CantonOrChief {
  case Canton
  case Chief
}

object CantonOrChief {
  def repr(c: CantonOrChief): Set[String] = c match {
    case Canton => Set("canton")
    case Chief => Set("chief")
  }

  val reprs: Map[CantonOrChief, Set[String]] = CantonOrChief.values.map(c => (c, repr(c))).toMap
}