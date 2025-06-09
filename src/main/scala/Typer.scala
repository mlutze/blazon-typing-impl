import grammar.*
import grammar.terminal.{Accent, Attitude, Cardinal, MobileCharge, Ordinal, Tincture, Variation}
import org.bitbucket.inkytonik.kiama.util.{Position, Positions}

import scala.collection.mutable.ArrayBuffer

object Typer {

  type Location = (Position, Position)

  enum Error extends RuntimeException {
    val loc: Location
    case RepeatedTincture(loc: Location)
    case UndefinedTinctureReference(loc: Location)
    case RepeatedCount(loc: Location)
    case UndefinedCountReference(loc: Location)
    case RuleOfTinctureSingle(loc: Location)
    case RuleOfTinctureMixed(loc: Location)
    case TrivialVariation(loc: Location)
    case OddVariation(loc: Location)
    case ImpossibleAlignment(loc: Location)
    case InvalidAttitudeOrAccent(loc: Location)
  }

  case class Context(bg: Background)

  case class State(tincs: ArrayBuffer[Tincture], var field: Option[Tincture], var count: Option[Int])

  object State {
    def init(): State = State(ArrayBuffer.empty, None, None)
  }

  enum Background {
    case Undivided(tex: Texture)
    case Divided(tex1: Texture, tex2: Texture)
  }

  object Background {
    def tincs(bg: Background): List[Tincture] = bg match {
      case Background.Undivided(tex) => Texture.tincs(tex)
      case Background.Divided(tex1, tex2) => Texture.tincs(tex1) ::: Texture.tincs(tex2)
    }
  }

  enum Texture {
    // NB: semy does not count as texture
    case Single(tinc: ![Tincture])
    case Mixed(tinc1: ![Tincture], tinc2: ![Tincture])
  }

  object Texture {
    def tincs(tex: Texture): List[Tincture] = tex match {
      case Texture.Single(tinc) => List(tinc.!)
      case Texture.Mixed(tinc1, tinc2) => List(tinc1.!, tinc2.!)
    }
  }


  def check(b: Blazon)(implicit pos: Positions): Unit = {
    implicit val state: State = State.init()
    b match {
      case Blazon(field, ch, more, cant, over, bord) =>
        val bg = checkField(field)
        ch.foreach(checkChargeEtc(_, bg))
        more.foreach(checkChargeLocated(_, bg)) // TODO bg relative to position
        cant.foreach(checkCantonOrChiefEtc(_, bg))
        over.foreach(checkChargeEtc(_, bg)) // maybe this bg should include charge or canton
        bord.foreach(checkBordureDescribed(_, bg)) // TODO check bordure rules
    }
  }

  def checkField(field: Field)(implicit state: State, pos: Positions): Background = field match {
    case Field.Divided(_, fill1, fill2) =>
      // ignore division: nothing can go wrong there
      // TODO check must be different
      val tex1 = checkFill(fill1)
      val tex2 = checkFill(fill2)
      Background.Divided(tex1, tex2)

    case Field.Undivided(fill) =>
      val tex = checkFill(fill)
      // TODO maybe a proper error here
      state.field = Texture.tincs(tex) match {
        case head :: Nil => Some(head)
        case _ => None //
      }
      Background.Undivided(tex)
  }

  def checkFill(fill: Fill)(implicit state: State, pos: Positions): Texture = fill match {
    case Fill.Flat(tinc, semy) =>
      val t = checkTinctureRef(tinc)
      val tex = Texture.Single(t)
      val bg = Background.Undivided(tex)
      semy.foreach(checkSemy(_, bg))
      tex
    case Fill.Variated(va, card, tinc1, tinc2) =>
      card.foreach {
        case c =>
          val n = Cardinal.value(c.!)
          if (n % 2 != 0) {
            throw Error.OddVariation(getLocation(c))
          }
          if (!Variation.canDisplay(va, n)) {
            throw Error.TrivialVariation(getLocation(c))
          }
      }
      val t1 = checkTinctureRef(tinc1)
      val t2 = checkTinctureRef(tinc2)
      Texture.Mixed(t1, t2)
  }

  def checkTinctureRef(tincRef: TinctureRef)(implicit state: State, pos: Positions): ![Tincture] = tincRef match {
    case grammar.TinctureRef.Direct(tinc) =>
      if (state.tincs.contains(tinc)) {
        throw Error.RepeatedTincture(getLocation(tincRef))
      }
      if (tinc != Tincture.Proper) {
        state.tincs.append(tinc)
      }
      boxFrom(tinc, tincRef)
    case grammar.TinctureRef.OrdReference(ord) =>
      // Scala is zero-indexed but blazon is 1-indexed
      if (!state.tincs.isDefinedAt(Ordinal.toInt(ord) - 1)) {
        throw Error.UndefinedTinctureReference(getLocation(tincRef))
      }
      boxFrom(state.tincs(Ordinal.toInt(ord) - 1), tincRef)
    case grammar.TinctureRef.LastReference() =>
      if (state.tincs.isEmpty) {
        throw Error.UndefinedTinctureReference(getLocation(tincRef))
      }
      boxFrom(state.tincs.last, tincRef)
    case grammar.TinctureRef.FieldReference() =>
      if (state.field.isEmpty) {
        throw Error.UndefinedTinctureReference(getLocation(tincRef))
      }
      boxFrom(state.field.get, tincRef)
  }

  def checkCount(c: Count)(implicit state: State, pos: Positions): Int = c match {
    case grammar.Count.A() => 1
    case grammar.Count.An() => 1
    case grammar.Count.The() => 1
    case grammar.Count.N(card) =>
      val n = Cardinal.value(card)
      state.count match {
        case Some(oldN) if oldN == n => Error.RepeatedCount(getLocation(c))
        case _ => state.count = Some(n)
      }
      n
    case grammar.Count.AsMany() => state.count match {
      case None => throw Error.UndefinedCountReference(getLocation(c))
      case Some(n) => n
    }
  }

  def checkSemy(semy: Semy, bg: Background)(implicit state: State, pos: Positions): Unit = semy match
    case grammar.Semy.Primitive(_, tinc) =>
      val t = tinc.map(checkTinctureRef)
      t.foreach(fg => checkRuleOfTincture(Texture.Single(fg), bg))
    case grammar.Semy.Of(ch) =>
      checkChargeDescribed(Integer.MAX_VALUE, ch, bg)

  def checkChargeEtc(ch: ChargeEtc, bg: Background)(implicit state: State, pos: Positions): Option[Background] = ch match {
    case ChargeEtc.Surcharge(bt, n, ch) =>
      // TODO re-stratification or something...
      val bg1 = checkChargeEtc(bt, bg)
      val i = checkCount(n)
      checkChargeDescribed(i, ch, bg1.get) // TODO unsafe get
    case ChargeEtc.Principle(n, ch, surr) =>
      val i = checkCount(n)
      val bg1 = checkChargeDescribed(i, ch, bg)
      val bg2 = surr.flatMap(checkSurroundingCharges(_, bg))
      bg1.orElse(bg2)
  }

  def checkSurroundingCharges(surr: SurroundingCharges, bg: Background)(implicit state: State, pos: Positions): Option[Background] = surr match {
    case SurroundingCharges.Uniform(count, ch) =>
      val i = checkCount(count)
      checkChargeDescribed(i, ch, bg)
    case SurroundingCharges.Divided(ch1, ch2) =>
      val bg1 = checkChargeLocated(ch1, bg)
      val bg2 = checkChargeLocated(ch2, bg)
      bg1.orElse(bg2)
      // TODO might need to divide/focus background
  }

  def checkChargeLocated(ch: ChargeLocated, bg: Background)(implicit state: State, pos: Positions): Option[Background] = ch match {
    case ChargeLocated(loc, count, ch) =>
      val i = checkCount(count)
      checkChargeDescribed(i, ch, bg)
  }

  def checkChargeDescribed(n: Int, ch: ChargeDescribed, bg: Background)(implicit state: State, pos: Positions): Option[Background] = ch match {
    case ChargeDescribed.Ordinary(ord) =>
      checkOrdinaryDescribed(ord, bg)
    case ChargeDescribed.Mobile(ch) =>
      checkMobileChargeDescribed(n, ch, bg)
  }

  def checkOrdinaryDescribed(ord: OrdinaryDescribed, bg: Background)(implicit state: State, pos: Positions): Option[Background] = ord match {
    case OrdinaryDescribed(ord, ln, fill) =>
      // can ignore ordinary and line
      // nothing goes wrong there
      fill.map(checkFill).map(Background.Undivided(_))
  }

  def checkRuleOfTincture(tex: Texture, bg: Background)(implicit state: State, pos: Positions): Unit = {
    tex match {
      // we have a problem if the entire background is incompatible with the single tincture
      case Texture.Single(tinc) =>
        if (Background.tincs(bg).forall(!areCompatible(tinc.!, _))) {
          throw Error.RuleOfTinctureSingle(getLocation(tinc))
        }
      // we have a problem if the entire background is incompatible with the primary texture color
      case Texture.Mixed(tinc1, tinc2) =>
        if (Background.tincs(bg).forall(!areCompatible(tinc1.!, _))) {
          throw Error.RuleOfTinctureMixed(getLocation(tinc1))
        }
    }
  }


  def areCompatible(tinc1: Tincture, tinc2: Tincture): Boolean = (Tincture.kind(tinc1), Tincture.kind(tinc2)) match {
    case (Tincture.Kind.Metal, Tincture.Kind.Metal) => false
    case (Tincture.Kind.Colour, Tincture.Kind.Colour) => false
    case (_, _) => true
  }

  def checkCantonOrChiefEtc(cant: CantonOrChiefEtc, bg: Background)(implicit state: State, pos: Positions): Unit = cant match {
    case CantonOrChiefEtc.Surcharge(c, n, ch) =>
      val bg1 = checkCantonOrChiefDescribed(c, bg)
      val i = checkCount(n)
      checkChargeDescribed(i, ch, bg1.get) // TODO unsafe get
    case CantonOrChiefEtc.Principle(c) =>
      checkCantonOrChiefDescribed(c, bg)
  }

  def checkCantonOrChiefDescribed(cant: CantonOrChiefDescribed, bg: Background)(implicit state: State, pos: Positions): Option[Background] = cant match {
    case CantonOrChiefDescribed(c, ln, fill) =>
      // ignore cant and ln; nothing can go wrong here
      val tex = fill.map(checkFill)
      tex.foreach(checkRuleOfTincture(_, bg))
      tex.map(Background.Undivided(_))
  }

  def checkBordureDescribed(bord: BordureDescribed, bg: Background)(implicit state: State, pos: Positions): Unit = bord match {
    case BordureDescribed(_, fill) =>
      // line cannot go wrong
      val tex = checkFill(fill)
      checkRuleOfTincture(tex, bg)
  }

  def checkMobileChargeDescribed(n: Int, mch: MobileChargeDescribed, bg: Background)(implicit state: State, pos: Positions): Option[Background] = mch match {
    case MobileChargeDescribed(ch, atts, al, fill, accs) =>
      atts.foreach(checkAttitude(ch.!, _))
      val tex = fill.map(checkFill).orElse(
        MobileCharge.implicitTincture(ch.!)
          .map(t => Texture.Single(boxFrom(t, ch))))
      tex.foreach(checkRuleOfTincture(_, bg))
      al.foreach {
        a => if (!Alignment.canDisplay(a.!, n)) {
          throw Error.ImpossibleAlignment(getLocation(a))
        }
      }
      accs.foreach(checkAccentGroup(ch.!, _))
      tex.map(Background.Undivided(_))
  }

  def checkAttitude(ch: MobileCharge, att: ![Attitude])(implicit pos: Positions): Unit = {
    if (!AttitudesAndAccents.attitudes(ch).contains(att.!)) {
      throw Error.InvalidAttitudeOrAccent(getLocation(att))
    }
  }

  def checkAccentGroup(ch: MobileCharge, acc: AccentGroup)(implicit state: State, pos: Positions): Unit = acc match {
    case AccentGroup(accs, tinc) =>
      accs.foreach(checkAccent(ch, _))
      checkTinctureRef(tinc)
  }
  
  def checkAccent(ch: MobileCharge, acc: ![Accent])(implicit pos: Positions): Unit = {
    if (!AttitudesAndAccents.accents(ch).contains(acc.!)) {
      throw Error.InvalidAttitudeOrAccent(getLocation(acc))
    }
  }

  def getLocation(node: Nonterminal)(implicit pos: Positions): Location = {
    (pos.getStart(node).get, pos.getFinish(node).get)
  }
  
  def boxFrom[A](a: A, node: Nonterminal)(implicit pos: Positions): ![A] = {
    pos.dupPos(node, !.apply(a))
  }

}
