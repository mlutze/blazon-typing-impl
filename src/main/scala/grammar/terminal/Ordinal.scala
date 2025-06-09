package grammar.terminal

enum Ordinal {
  case First
  case Second
  case Third
  case Fourth
  case Fifth
  case Sixth
  case Seventh
  case Eighth
  case Ninth
  case Tenth
}

object Ordinal {
  def repr(c: Ordinal): Set[String] = c match {
    case First => Set("first")
    case Second => Set("second")
    case Third => Set("third")
    case Fourth => Set("fourth")
    case Fifth => Set("fifth")
    case Sixth => Set("sixth")
    case Seventh => Set("seventh")
    case Eighth => Set("eighth")
    case Ninth => Set("ninth")
    case Tenth => Set("tenth")
  }

  val reprs: Map[Ordinal, Set[String]] = Ordinal.values.map(c => (c, repr(c))).toMap
  
  def toInt(o: Ordinal): Int = o match
    case Ordinal.First => 1
    case Ordinal.Second => 2
    case Ordinal.Third => 3
    case Ordinal.Fourth => 4
    case Ordinal.Fifth => 5
    case Ordinal.Sixth => 6
    case Ordinal.Seventh => 7
    case Ordinal.Eighth => 8
    case Ordinal.Ninth => 9
    case Ordinal.Tenth => 10
}
