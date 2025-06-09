import grammar.terminal
import grammar.terminal.Accent.*
import grammar.terminal.Attitude.*
import grammar.terminal.{Accent, Attitude, MobileCharge}

object AttitudesAndAccents {

  def attitudes(ch: MobileCharge): Set[Attitude] = ch match {
    case MobileCharge.Anchor => Set()
    case MobileCharge.Annulet => Set()
    case MobileCharge.Arrow => Set(Bendwise)
    case MobileCharge.Bar => Set(Voided, Gemel)
    case MobileCharge.Barbel => Set(Bendwise)
    case MobileCharge.Baston => Set()
    case MobileCharge.Bezant => Set()
    case MobileCharge.Billet => Set()
    case MobileCharge.Bird => Set()
    case MobileCharge.Boar => Set(Passant, Statant, Combatant, Bendwise)
    case MobileCharge.BoarsHead => Set(Erased, Couped, Cabossed)
    case MobileCharge.Buck => Set(Trippant)
    case MobileCharge.BucksHead => Set(Cabossed, Erased, Couped)
    case MobileCharge.Buckle => Set()
    case MobileCharge.Buglehorn => Set()
    case MobileCharge.Chaplet => Set()
    case MobileCharge.Cock => Set()
    case MobileCharge.Moorcock => Set()
    case MobileCharge.Column => Set(Bendwise)
    case MobileCharge.CornishChough => Set()
    case MobileCharge.CoveredCup => Set()
    case MobileCharge.Crane => Set()
    case MobileCharge.Crescent => Set()
    case MobileCharge.Cross => Set(
      Botonny,
      Couped,
      Crosslet,
      Engrailed,
      Fitchy,
      Flory,
      Formy,
      Humetty,
      Moline,
      Patonce,
      Patty,
      Pomelly,
      Sarcelly,
      Tau,
      Voided,
    )
    case MobileCharge.Crosslet => Set(Fitchy)
    case MobileCharge.Crow => Set()
    case MobileCharge.Cup => Set()
    case MobileCharge.DemiLion => Set(
      Addorsed,
      Bendwise,
      Combatant,
      Couped,
      Erased, Couped,
      Guardant, Reguardant,
      Passant, Statant,
      Rampant,
    )
    case MobileCharge.DexterHand => Set()
    case MobileCharge.Dolphin => Set(
      Bendwise,
      Embowed,
      Haurient,
      Naiant,
    )
    case MobileCharge.DucalCrown => Set()
    case MobileCharge.Eagle => Set(
      Ayrant,
      Bendwise,
      Displayed,
      DoubleHeaded,
      Volant,
    )
    case MobileCharge.ElephantsHead => Set(Erased, Couped, Cabossed)
    case MobileCharge.ErmineSpot => Set()
    case MobileCharge.Escallop => Set()
    case MobileCharge.Escucheon => Set()
    case MobileCharge.Estoile => Set(Pierced)
    case MobileCharge.Falcon => Set(Volant)
    case MobileCharge.Fish => Set(
      Bendwise,
      Embowed,
      Haurient,
      Naiant,
    )
    case MobileCharge.FiveFoil => Set(Pierced)
    case MobileCharge.FleurDeLis => Set()
    case MobileCharge.Flintstone => Set()
    case MobileCharge.Fusil => Set()
    case MobileCharge.Garb => Set()
    case MobileCharge.GoatsHead => Set(Erased, Couped, Cabossed)
    case MobileCharge.Goshawk => Set()
    case MobileCharge.Greyhound => Set(Passant, Statant, Courant, Bendwise)
    case MobileCharge.GreyhoundsHead => Set(Erased, Couped, Cabossed)
    case MobileCharge.Griffin => Set(Passant, Statant, Combatant, Bendwise)
    case MobileCharge.GriffinsHead => Set(Cabossed, Erased, Couped)
    case MobileCharge.Hautboy => Set(Bendwise)
    case MobileCharge.Hawk => Set(Volant)
    case MobileCharge.Heart => Set()
    case MobileCharge.HorsesHead => Set(Erased, Couped, Cabossed)
    case MobileCharge.Horseshoe => Set()
    case MobileCharge.Hurt => Set()
    case MobileCharge.Lapwing => Set()
    case MobileCharge.LeopardsHead => Set(Cabossed, Erased, Couped)
    case MobileCharge.Lion => Set(
      Addorsed,
      Bendwise,
      Cabossed, Erased, Couped,
      Combatant,
      Couchant,
      Guardant, Reguardant,
      Passant, Statant,
      Rampant,
    )
    case MobileCharge.LionsHead => Set(Erased, Couped, Cabossed, Erased, Couped)
    case MobileCharge.Lozenge => Set()
    case MobileCharge.LozengeBuckle => Set()
    case MobileCharge.Martlet => Set(Volant)
    case MobileCharge.Mascle => Set()
    case MobileCharge.Millstone => Set()
    case MobileCharge.Mullet => Set(Pierced)
    case MobileCharge.NagsHead => Set(Erased, Couped, Cabossed, Erased, Couped)
    case MobileCharge.Nail => Set(Bendwise)
    case MobileCharge.Ogress => Set()
    case MobileCharge.Orle => Set(Engrailed)
    case MobileCharge.Owl => Set()
    case MobileCharge.Pen => Set()
    case MobileCharge.Pewit => Set()
    case MobileCharge.Pheon => Set()
    case MobileCharge.Pillar => Set(Bendwise)
    case MobileCharge.Pipe => Set(Bendwise)
    case MobileCharge.Pitcher => Set()
    case MobileCharge.Plate => Set()
    case MobileCharge.Plough => Set()
    case MobileCharge.Portcullis => Set()
    case MobileCharge.Raven => Set()
    case MobileCharge.Rock => Set()
    case MobileCharge.Rose => Set()
    case MobileCharge.RoundBuckle => Set()
    case MobileCharge.SinisterHand => Set()
    case MobileCharge.SixFoil => Set(Pierced)
    case MobileCharge.Spearhead => Set()
    case MobileCharge.SquareBuckle => Set()
    case MobileCharge.Stag => Set(Trippant)
    case MobileCharge.StagsHead => Set(Cabossed, Erased, Couped, Couped)
    case MobileCharge.StandingCup => Set()
    case MobileCharge.Star => Set()
    case MobileCharge.Stork => Set()
    case MobileCharge.SugarLoaf => Set()
    case MobileCharge.Sun => Set(Radiated, InSplendour)
    case MobileCharge.Swan => Set()
    case MobileCharge.Sword => Set(Bendwise)
    case MobileCharge.Talbot => Set(Passant, Statant, Combatant, Bendwise)
    case MobileCharge.TalbotsHead => Set(Erased, Couped, Cabossed, Erased, Couped, Bendwise)
    case MobileCharge.Temple => Set()
    case MobileCharge.ThreeFoil => Set(Slipped, Pierced)
    case MobileCharge.Tombstone => Set()
    case MobileCharge.Torteau => Set()
    case MobileCharge.UnicornsHead => Set(Cabossed, Erased, Couped)
    case MobileCharge.WallStone => Set()
    case MobileCharge.Windmill => Set()
  }

  def accents(ch: MobileCharge): Set[Accent] = ch match {
    case MobileCharge.Anchor => Set()
    case MobileCharge.Annulet => Set()
    case MobileCharge.Arrow => Set()
    case MobileCharge.Bar => Set()
    case MobileCharge.Barbel => Set()
    case MobileCharge.Baston => Set()
    case MobileCharge.Bezant => Set()
    case MobileCharge.Billet => Set()
    case MobileCharge.Bird => Set(Beaked, Membered, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.Boar => Set(Crowned, DucallyCrowned, Armed, Collared, Gorged, DucallyGorged)
    case MobileCharge.BoarsHead => Set(Crowned, DucallyCrowned, Armed)
    case MobileCharge.Buck => Set(Crowned, DucallyCrowned, Attired, Collared, Gorged, DucallyGorged)
    case MobileCharge.BucksHead => Set(Crowned, DucallyCrowned, Attired)
    case MobileCharge.Buckle => Set()
    case MobileCharge.Buglehorn => Set(Stringed)
    case MobileCharge.Chaplet => Set()
    case MobileCharge.Cock => Set(Beaked, Membered, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged, Legged)
    case MobileCharge.Moorcock => Set(Beaked, Membered, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged, Legged)
    case MobileCharge.Column => Set()
    case MobileCharge.CornishChough => Set(Beaked, Membered, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.CoveredCup => Set()
    case MobileCharge.Crane => Set(Beaked, Membered, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.Crescent => Set()
    case MobileCharge.Cross => Set()
    case MobileCharge.Crosslet => Set()
    case MobileCharge.Crow => Set(Beaked, Membered, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.Cup => Set()
    case MobileCharge.DemiLion => Set(Langued, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.DexterHand => Set()
    case MobileCharge.Dolphin => Set(Crowned, DucallyCrowned)
    case MobileCharge.DucalCrown => Set()
    case MobileCharge.Eagle => Set(Membered, Legged,
      Armed,
      Beaked,
      Crowned,
      DucallyCrowned,
      Legged, Collared, Gorged, DucallyGorged
    )
    case MobileCharge.ErmineSpot => Set()
    case MobileCharge.Escallop => Set()
    case MobileCharge.Escucheon => Set()
    case MobileCharge.Estoile => Set()
    case MobileCharge.Falcon => Set(Membered, Legged, Beaked, Legged, Crowned, DucallyCrowned, Armed, Collared, Gorged, DucallyGorged)
    case MobileCharge.Fish => Set(Crowned, DucallyCrowned)
    case MobileCharge.FiveFoil => Set()
    case MobileCharge.FleurDeLis => Set()
    case MobileCharge.Flintstone => Set()
    case MobileCharge.Fusil => Set()
    case MobileCharge.Garb => Set()
    case MobileCharge.GoatsHead => Set(Crowned, DucallyCrowned)
    case MobileCharge.Goshawk => Set(Beaked, Membered, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.Greyhound => Set(Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.GreyhoundsHead => Set(Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.Griffin => Set(Beaked, Crowned, DucallyCrowned, Armed, Collared, Gorged, DucallyGorged)
    case MobileCharge.GriffinsHead => Set(Beaked, Crowned, DucallyCrowned, Armed)
    case MobileCharge.Hautboy => Set()
    case MobileCharge.Hawk => Set(Membered, Legged, Beaked, Legged, Crowned, DucallyCrowned, Armed, Collared, Gorged, DucallyGorged)
    case MobileCharge.Heart => Set()
    case MobileCharge.HorsesHead => Set(Bridled, Crowned, DucallyCrowned)
    case MobileCharge.Horseshoe => Set()
    case MobileCharge.Hurt => Set()
    case MobileCharge.Lapwing => Set(Membered, Legged, Collared, Gorged, DucallyGorged)
    case MobileCharge.LeopardsHead => Set(Crowned, DucallyCrowned, Armed)
    case MobileCharge.Lion => Set(Crowned, Langued, Armed, DucallyCrowned, Armed, Collared, Gorged, DucallyGorged)
    case MobileCharge.LionsHead => Set(Crowned, DucallyCrowned, Armed)
    case MobileCharge.Lozenge => Set()
    case MobileCharge.LozengeBuckle => Set()
    case MobileCharge.Martlet => Set(Beaked, Membered, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.Mascle => Set()
    case MobileCharge.Millstone => Set()
    case MobileCharge.Mullet => Set()
    case MobileCharge.NagsHead => Set(Bridled, Crowned, DucallyCrowned)
    case MobileCharge.Nail => Set()
    case MobileCharge.Ogress => Set()
    case MobileCharge.Orle => Set()
    case MobileCharge.Owl => Set(Membered, Legged, Beaked, Legged, Crowned, DucallyCrowned, Armed, Collared, Gorged, DucallyGorged)
    case MobileCharge.Pen => Set()
    case MobileCharge.Pewit => Set(Beaked, Membered, Legged, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.Pheon => Set()
    case MobileCharge.Pillar => Set()
    case MobileCharge.Pipe => Set()
    case MobileCharge.Pitcher => Set()
    case MobileCharge.Plate => Set()
    case MobileCharge.Plough => Set()
    case MobileCharge.Portcullis => Set(Chained)
    case MobileCharge.Raven => Set(Armed, Membered, Legged, Beaked, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.Rock => Set()
    case MobileCharge.Rose => Set(Leaved)
    case MobileCharge.RoundBuckle => Set()
    case MobileCharge.SinisterHand => Set()
    case MobileCharge.SixFoil => Set()
    case MobileCharge.Spearhead => Set()
    case MobileCharge.SquareBuckle => Set()
    case MobileCharge.Stag => Set(Crowned, DucallyCrowned, Attired, Collared, Gorged, DucallyGorged)
    case MobileCharge.StagsHead => Set(Crowned, DucallyCrowned, Attired, Collared, Gorged, DucallyGorged)
    case MobileCharge.StandingCup => Set()
    case MobileCharge.Star => Set()
    case MobileCharge.Stork => Set(Armed, Membered, Legged, Beaked, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.SugarLoaf => Set()
    case MobileCharge.Sun => Set()
    case MobileCharge.Swan => Set(Membered, Legged, Beaked, Membered, Legged, Legged, Crowned, DucallyCrowned, Collared, Gorged, DucallyGorged)
    case MobileCharge.Sword => Set()
    case MobileCharge.Talbot => Set(Armed, Crowned, DucallyCrowned, Collared, Collared, Gorged, DucallyGorged)
    case MobileCharge.TalbotsHead => Set(Armed, Crowned, DucallyCrowned)
    case MobileCharge.Temple => Set()
    case MobileCharge.ThreeFoil => Set()
    case MobileCharge.Tombstone => Set()
    case MobileCharge.Torteau => Set()
    case MobileCharge.UnicornsHead => Set(Crowned, DucallyCrowned)
    case MobileCharge.WallStone => Set()
    case MobileCharge.Windmill => Set()
  }
}
