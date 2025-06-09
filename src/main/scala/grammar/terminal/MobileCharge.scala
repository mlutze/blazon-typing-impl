package grammar.terminal

enum MobileCharge {
  case Anchor
  case Annulet
  case Arrow
  case Bar
  case Barbel
  case Baston
  case Bezant
  case Billet
  case Bird
  case Boar
  case BoarsHead
  case Buck
  case BucksHead
  case Buckle
  case Buglehorn
  case Chaplet
  case Cock
  case Column
  case CornishChough
  case CoveredCup
  case Crane
  case Crescent
  case Cross
  case Crosslet
  case Crow
  case Crown
  case Cup
  case DemiLion
  case DexterHand
  case Dolphin
  case DucalCrown
  case Eagle
  case ElephantsHead
  case ErmineSpot
  case Escallop
  case Escucheon
  case Estoile
  case Falcon
  case Fish
  case FiveFoil
  case FleurDeLis
  case Flintstone
  case Fusil
  case Garb
  case GoatsHead
  case Goshawk
  case Greyhound
  case GreyhoundsHead
  case Griffin
  case GriffinsHead
  case Hautboy
  case Hawk
  case Heart
  case HorsesHead
  case Horseshoe
  case Hurt
  case Lapwing
  case LeopardsHead
  case Lion
  case LionsHead
  case Lozenge
  case LozengeBuckle
  case Martlet
  case Mascle
  case Millstone
  case Moorcock
  case Mullet
  case NagsHead
  case Nail
  case Ogress
  case Orle
  case Owl
  case Pen
  case Pewit
  case Pheon
  case Pillar
  case Pipe
  case Pitcher
  case Plate
  case Plough
  case Portcullis
  case Raven
  case Rock
  case Rose
  case RoundBuckle
  case SinisterHand
  case SixFoil
  case Spearhead
  case SquareBuckle
  case Stag
  case StagsHead
  case StandingCup
  case Star
  case Stork
  case SugarLoaf
  case Sun
  case Swan
  case Sword
  case Talbot
  case TalbotsHead
  case Temple
  case ThreeFoil
  case Tombstone
  case Torteau
  case UnicornsHead
  case WallStone
  case Windmill
}

object MobileCharge {

  def repr(s: MobileCharge): Set[String] = s match {
    case Anchor => Set("anchor", "anchors")
    case Annulet => Set("annulet", "annulets")
    case Arrow => Set("arrow", "arrows")
    case Bar => Set("bar", "bars")
    case Barbel => Set("barbel", "barbels")
    case Baston => Set("baston", "bastons")
    case Bezant => Set("bezant", "bezants")
    case Billet => Set("billet", "billets")
    case Bird => Set("bird", "birds")
    case Boar => Set("boar", "boars")
    case BoarsHead => Set("boar's head", "boar's heads")
    case Buck => Set("buck", "bucks")
    case BucksHead => Set("buck's", "buck's heads")
    case Buckle => Set("buckle", "buckles")
    case Buglehorn => Set("buglehorn", "buglehorns")
    case Chaplet => Set("chaplet", "chaplets")
    case Cock => Set("cock", "cocks")
    case Column => Set("column", "columns")
    case CornishChough => Set("Cornish chough", "Cornish choughs")
    case CoveredCup => Set("covered cup", "covered cups")
    case Crane => Set("crane", "cranes")
    case Crescent => Set("crescent", "crescents")
    case Cross => Set("cross", "crosses")
    case Crosslet => Set("crosslet", "crosslets")
    case Crow => Set("crow", "crows")
    case Crown => Set("crown", "crowns")
    case Cup => Set("cup", "cups")
    case DemiLion => Set("demi-lion", "demi-lions", "demi lion", "demi-lions")
    case DexterHand => Set("dexter hand", "dexter hands")
    case Dolphin => Set("dolphin", "dolphins")
    case DucalCrown => Set("ducal crown", "ducal crowns")
    case Eagle => Set("eagle", "eagles")
    case ElephantsHead => Set("elephant's head", "elephant's heads")
    case ErmineSpot => Set("ermine spot", "ermine spots")
    case Escallop => Set("escallop", "escallops")
    case Escucheon => Set("escucheon", "escucheons", "escu.")
    case Estoile => Set("estoile", "estoiles")
    case Falcon => Set("falcon", "falcons")
    case Fish => Set("fish", "fishes")
    case FiveFoil => Set("5-foil", "5-foils", "cinquefoil", "cinquefoils")
    case FleurDeLis => Set("fleur-de-lis", "fleurs-de-lis")
    case Flintstone => Set("flintstone", "flintstones")
    case Fusil => Set("fusil", "fusils")
    case Garb => Set("garb", "garbs")
    case GoatsHead => Set("goat's head", "goat's heads")
    case Goshawk => Set("goshawk", "goshawks")
    case Greyhound => Set("greyhound", "greyhounds")
    case GreyhoundsHead => Set("greyhound's head", "greyhound's heads")
    case Griffin => Set("griffin", "griffins")
    case GriffinsHead => Set("griffin's head", "griffin's heads")
    case Hautboy => Set("hautboy", "hautboys")
    case Hawk => Set("hawk", "hawks")
    case Heart => Set("heart", "hearts")
    case HorsesHead => Set("horse's head", "horse's heads")
    case Horseshoe => Set("horseshoe", "horseshoes")
    case Hurt => Set("hurt", "hurts")
    case Lapwing => Set("lapwing", "lapwings")
    case LeopardsHead => Set("leopard's head", "leopard's heads")
    case Lion => Set("lion", "lions")
    case LionsHead => Set("lion's head", "lion's heads")
    case Lozenge => Set("lozenge", "lozenges")
    case LozengeBuckle => Set("lozenge buckle", "lozenge buckles")
    case Martlet => Set("martlet", "martlets")
    case Mascle => Set("mascle", "mascles")
    case Millstone => Set("millstone", "millstones")
    case Moorcock => Set("moorcock", "moorcocks")
    case Mullet => Set("mullet", "mullets")
    case NagsHead => Set("nag's head", "nag's heads")
    case Nail => Set("nail", "nails")
    case Ogress => Set("ogress", "ogresses", "pellet", "pellets")
    case Orle => Set("orle", "orles")
    case Owl => Set("owl", "owls")
    case Pen => Set("pen", "pens")
    case Pewit => Set("pewit", "pewits")
    case Pheon => Set("pheon", "pheons")
    case Pillar => Set("pillar", "pillars")
    case Pipe => Set("pipe", "pipes")
    case Pitcher => Set("pitcher", "pitchers")
    case Plate => Set("plate", "plates")
    case Plough => Set("plough", "ploughs")
    case Portcullis => Set("portcullis", "portcullises")
    case Raven => Set("raven", "ravens")
    case Rock => Set("rock", "rocks")
    case Rose => Set("rose", "roses")
    case RoundBuckle => Set("round buckle", "round buckles")
    case SinisterHand => Set("sinister hand", "sinister hands")
    case SixFoil => Set("six-foil", "six-foils")
    case Spearhead => Set("spearhead", "spearheads")
    case SquareBuckle => Set("square buckle", "square buckles")
    case Stag => Set("stag", "stags")
    case StagsHead => Set("stag's head", "stag's heads")
    case StandingCup => Set("standing cup", "standing cups")
    case Star => Set("star", "stars")
    case Stork => Set("stork", "storks")
    case SugarLoaf => Set("sugar loaf", "sugar loaves")
    case Sun => Set("sun", "suns")
    case Swan => Set("swan", "swans")
    case Sword => Set("sword", "swords")
    case Talbot => Set("talbot", "talbots")
    case TalbotsHead => Set("talbot's head", "talbot's heads")
    case Temple => Set("temple", "temples")
    case ThreeFoil => Set("3-foil", "3-foils")
    case Tombstone => Set("tombstone", "tombstones")
    case Torteau => Set("torteau", "torteaux")
    case UnicornsHead => Set("unicorn's head", "unicorn's heads")
    case WallStone => Set("wall stone", "wall stones")
    case Windmill => Set("windmill", "windmills")
  }
  
  val reprs: Map[MobileCharge, Set[String]] = MobileCharge.values.map(c => (c, repr(c))).toMap

  def implicitTincture(c: MobileCharge): Option[Tincture] = c match {
    case Bezant => Some(Tincture.Or)
    case Hurt => Some(Tincture.Azure)
    case Ogress => Some(Tincture.Sable)
    case Plate => Some(Tincture.Argent)
    case Torteau => Some(Tincture.Gules)
    case _ => None
  }
}