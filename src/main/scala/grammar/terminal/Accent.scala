package grammar.terminal

enum Accent {
  case Armed
  case Attired
  case Beaked
  case Bridled
  case Chained
  case Collared
  case Crowned
  case DucallyCrowned
  case DucallyGorged
  case Gorged
  case Langued
  case Leaved
  case Legged
  case Membered
  case Stalked
  case Stringed
}

object Accent {
  def repr(p: Accent): Set[String] = p match {
    case Armed => Set("armed")
    case Attired => Set("attired")
    case Beaked => Set("beaked")
    case Bridled => Set("bridled")
    case Chained => Set("chained")
    case Collared => Set("collared")
    case Crowned => Set("crowned")
    case DucallyCrowned => Set("ducally crowned")
    case Gorged => Set("gorged")
    case DucallyGorged => Set("ducally gorged")
    case Langued => Set("langued")
    case Leaved => Set("leaved")
    case Legged => Set("legged")
    case Membered => Set("membered")
    case Stalked => Set("stalked")
    case Stringed => Set("stringed")
  }

  val reprs: Map[Accent, Set[String]] = Accent.values.map(c => (c, repr(c))).toMap
}