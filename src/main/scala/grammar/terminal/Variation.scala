package grammar.terminal

enum Variation {
  case Barry
  case Bendy
  case Chequy
  case Compony // CHECK: only valid for ordinaries
  case Countercompony // CHECK: only valid for ordinaries
  case Gyronny
  case Lozengy
  case Paly
  case Vairy
}

object Variation {
  def repr(v: Variation): Set[String] = v match {
    case Barry => Set("barry")
    case Bendy => Set("bendy")
    case Compony => Set("compony")
    case Countercompony => Set("countercompony")
    case Lozengy => Set("lozengy")
    case Paly => Set("paly")
    case Chequy => Set("chequy")
    case Gyronny => Set("gyronny")
    case Vairy => Set("vairy")
  }

  val reprs: Map[Variation, Set[String]] = Variation.values.map(c => (c, repr(c))).toMap
  
  def canDisplay(v: Variation, n: Int): Boolean = v match {
    case Variation.Barry => n % 2 == 0 && n > 2
    case Variation.Bendy => n % 2 == 0 && n > 2
    case Variation.Chequy => false
    case Variation.Compony => false
    case Variation.Countercompony => false
    case Variation.Gyronny => n % 2 == 0 && n > 4
    case Variation.Lozengy => false
    case Variation.Paly => n % 2 == 0 && n > 2
    case Variation.Vairy => false
  }
}