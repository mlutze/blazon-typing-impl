package grammar.terminal

enum Cardinal {
  case One
  case Two
  case Three
  case Four
  case Five
  case Six
  case Seven
  case Eight
  case Nine
  case Ten
  case Eleven
  case Twelve
  case Thirteen
  case Fourteen
  case Fifteen
}

object Cardinal {
  def value(c: Cardinal): Int = c match {
    case Cardinal.One => 1
    case Cardinal.Two => 2
    case Cardinal.Three => 3
    case Cardinal.Four => 4
    case Cardinal.Five => 5
    case Cardinal.Six => 6
    case Cardinal.Seven => 7
    case Cardinal.Eight => 8
    case Cardinal.Nine => 9
    case Cardinal.Ten => 10
    case Cardinal.Eleven => 11
    case Cardinal.Twelve => 12
    case Cardinal.Thirteen => 13
    case Cardinal.Fourteen => 14
    case Cardinal.Fifteen => 15
  }
  
  def repr(c: Cardinal): Set[String] = c match {
    case One => Set("one")
    case Two => Set("two")
    case Three => Set("three")
    case Four => Set("four")
    case Five => Set("five")
    case Six => Set("six")
    case Seven => Set("seven")
    case Eight => Set("eight")
    case Nine => Set("nine")
    case Ten => Set("ten")
    case Eleven => Set("eleven")
    case Twelve => Set("twelve")
    case Thirteen => Set("thirteen")
    case Fourteen => Set("fourteen")
    case Fifteen => Set("fifteen")
  }

  val reprs: Map[Cardinal, Set[String]] = Cardinal.values.map(c => (c, repr(c))).toMap
}