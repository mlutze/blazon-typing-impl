package grammar.terminal

enum SemyPrimitive {
  case Crusily
  case Fretty
  case Bezanty
}
object SemyPrimitive {
  def repr(s: SemyPrimitive): Set[String] = s match {
    case Crusily => Set("crusily")
    case Fretty => Set("fretty")
    case Bezanty => Set("bezanty")
  }

  val reprs: Map[SemyPrimitive, Set[String]] = SemyPrimitive.values.map(c => (c, repr(c))).toMap
}
