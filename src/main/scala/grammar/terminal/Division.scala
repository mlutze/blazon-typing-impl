package grammar.terminal

enum Division {
  case PerPale
  case PerChevron
  case PerFess
  case PerQuarter
  case PerBend
}

object Division {
  def repr(d: Division): Set[String] = d match {
    case Division.PerPale => Set("per pale")
    case Division.PerChevron => Set("per chev.", "per chevron")
    case Division.PerFess => Set("per fess")
    case Division.PerQuarter => Set("per quarter", "quarterly")
    case Division.PerBend => Set("per bend")
  }
  
  val reprs: Map[Division, Set[String]] = Division.values.map(c => (c, repr(c))).toMap
}
