package grammar.terminal

enum Location {
  case Chief
  case Base
  case Dexter
  case Sinister
  case DexterChief
  case SinisterChief
  case DexterBase
  case SinisterBase
  case FirstQuarter
  case SecondQuarter 
  case ThirdQuarter
  case FourthQuarter
}

object Location {

  def repr(p: Location): Set[String] = p match {
    case Chief => Set("chief")
    case Base => Set("base")
    case Dexter => Set("dexter")
    case Sinister => Set("sinister")
    case DexterChief => Set("dexter chief")
    case SinisterChief => Set("sinister chief")
    case DexterBase => Set("dexter base")
    case SinisterBase => Set("sinister base")
    case FirstQuarter => Set("first quarter")
    case SecondQuarter => Set("second quarter")
    case ThirdQuarter => Set("third quarter")
    case FourthQuarter => Set("fourth quarter")
  }

  val reprs: Map[Location, Set[String]] = Location.values.map(c => (c, repr(c))).toMap
}