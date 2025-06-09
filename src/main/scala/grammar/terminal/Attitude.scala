package grammar.terminal

enum Attitude {
  case Addorsed
  case Ayrant
  case Bendwise
  case Botonny
  case Cabossed
  case Combatant
  case Couchant
  case Couped
  case Courant
  case Crosslet
  case Displayed
  case DoubleHeaded
  case Embowed
  case Engrailed
  case Erased
  case Erect
  case Fesswise
  case Fitchy
  case Flory
  case Formy
  case Garnished
  case Gemel
  case Guardant
  case Haurient
  case Humetty
  case InSplendour
  case Moline
  case Naiant
  case Palewise
  case Passant
  case Patonce
  case Patty
  case Pierced
  case Pomelly
  case Radiated
  case Rampant
  case Reguardant
  case Sarcelly
  case Slipped
  case Statant
  case Tau
  case Trippant
  case Voided // is voided rather a fill?
  case Volant
}

object Attitude {
  def repr(p: Attitude): Set[String] = p match {
    case Addorsed => Set("addorsed")
    case Ayrant => Set("ayrant")
    case Bendwise => Set("bendwise")
    case Botonny => Set("botonny")
    case Cabossed => Set("cabossed")
    case Combatant => Set("combatant")
    case Couchant => Set("couchant")
    case Couped => Set("couped")
    case Courant => Set("courant")
    case Crosslet => Set("croslet", "crosslet") // ERROR?
    case Displayed => Set("displayed", "displ.", "displ")
    case DoubleHeaded => Set("double headed")
    case Embowed => Set("embowed")
    case Engrailed => Set("engrailed", "engr.", "engr")
    case Erased => Set("erased")
    case Erect => Set("erect")
    case Fesswise => Set("fesswise")
    case Fitchy => Set("fitchy")
    case Flory => Set("flory")
    case Formy => Set("formy")
    case Garnished => Set("garnished")
    case Gemel => Set("gemel")
    case Guardant => Set("guardant", "guard.", "guard")
    case Haurient => Set("haurient")
    case Humetty => Set("humetty")
    case InSplendour => Set("in splendour", "in its splendour", "in glory", "in its glory")
    case Moline => Set("moline")
    case Naiant => Set("naiant")
    case Palewise => Set("palewise")
    case Passant => Set("passant", "pass.", "pass")
    case Patonce => Set("patonce")
    case Patty => Set("patty")
    case Pierced => Set("pierced")
    case Pomelly => Set("pomelly")
    case Radiated => Set("radiated")
    case Rampant => Set("rampant", "ramp.", "ramp")
    case Reguardant => Set("reguardant", "reguard.", "reguard")
    case Sarcelly => Set("sarcelly")
    case Slipped => Set("slipped")
    case Statant => Set("statant")
    case Tau => Set("tau")
    case Trippant => Set("trippant")
    case Voided => Set("voided")
    case Volant => Set("volant")
  }

  val reprs: Map[Attitude, Set[String]] = Attitude.values.map(c => (c, repr(c))).toMap
}