package grammar.terminal

enum Tincture {
  case Argent
  case Azure
  case Ermine
  case Erminois
  case Ermines
  case Gules
  case Or
  case Potent
  case Purpure
  case Sable
  case Vair
  case Vert
  case Proper
}

object Tincture {
  enum Kind {
    case Metal
    case Colour
    case Fur
    case Proper
  }

  def kind(t: Tincture): Kind = t match {
    case Tincture.Argent => Kind.Metal
    case Tincture.Or => Kind.Metal

    case Tincture.Azure => Kind.Colour
    case Tincture.Gules => Kind.Colour
    case Tincture.Purpure => Kind.Colour
    case Tincture.Sable => Kind.Colour
    case Tincture.Vert => Kind.Colour

    case Tincture.Ermine => Kind.Fur
    case Tincture.Ermines => Kind.Fur
    case Tincture.Erminois => Kind.Fur
    case Tincture.Potent => Kind.Fur
    case Tincture.Vair => Kind.Fur

    case Tincture.Proper => Kind.Proper
  }
  
  def repr(t: Tincture): Set[String] = t match {
    case Tincture.Argent => Set("argent", "arg.", "arg")
    case Tincture.Azure => Set("azure", "az.", "az")
    case Tincture.Ermine => Set("ermine", "erm.", "erm")
    case Tincture.Ermines => Set("ermines")
    case Tincture.Erminois => Set("erminois")
    case Tincture.Gules => Set("gules", "gu.", "gu")
    case Tincture.Or => Set("or")
    case Tincture.Potent => Set("potent")
    case Tincture.Purpure => Set("purpure", "purp.", "purp")
    case Tincture.Sable => Set("sable", "sa.", "sa")
    case Tincture.Vair => Set("vair")
    case Tincture.Vert => Set("vert")
    case Tincture.Proper => Set("proper", "ppr.", "ppr")
  }

  val reprs: Map[Tincture, Set[String]] = Tincture.values.map(t => (t, repr(t))).toMap
}