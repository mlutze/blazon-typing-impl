import grammar.*
import grammar.terminal.*
import org.bitbucket.inkytonik.kiama
import org.bitbucket.inkytonik.kiama.parsing.{Failure, Input, ListParsers, NoSuccess, ParseResult, Success}
import org.bitbucket.inkytonik.kiama.util.{Positions, StringSource}

class Parser(val pos: Positions = new Positions) extends ListParsers(pos) {

  /** primitives */
  private lazy val tincture = fromReprs("tincture", Tincture.reprs)
  private lazy val ordinary = fromReprs("charge", Ordinary.reprs) // just use charge for generality
  private lazy val line = fromReprs("line", Line.reprs)
  private lazy val division = fromReprs("division", Division.reprs)
  private lazy val variation = fromReprs("variation", Variation.reprs)
  private lazy val mobileCharge = fromReprs("charge", MobileCharge.reprs)
  private lazy val cardinal = fromReprs("cardinal", Cardinal.reprs)
  private lazy val semyPrimitive = fromReprs("semy", SemyPrimitive.reprs)
  private lazy val ordinal = fromReprs("ordinal", Ordinal.reprs)
  private lazy val position = fromReprs("position", Position.reprs)
  private lazy val cantonOrChief = fromReprs("charge", CantonOrChief.reprs) // just use chare for generality
  private lazy val attitude = fromReprs("attitutde", Attitude.reprs)
  private lazy val accent = fromReprs("accent", Accent.reprs)
  private lazy val location = fromReprs("location", Location.reprs)

  /** strings */
  private lazy val a = "the" | "an" | "a"
  private lazy val between = "between" | "betw."

  private lazy val count =
    "as many" ^^^ Count.AsMany() |
      "the" ^^^ Count.The() |
      "an" ^^^ Count.An() |
      "a" ^^^ Count.A() |
      cardinal ^^ Count.N.apply

  private lazy val ordinaryDescribed =
    ordinary ~ opt(line) ~ opt(fill) ^^ OrdinaryDescribed.apply

  private lazy val cantonOrChiefDescribed =
    cantonOrChief ~ opt(line) ~ opt(fill) ^^ CantonOrChiefDescribed.apply

  private lazy val divisionLined =
    division ~ opt(line) ^^ DivisionLined.apply

  private lazy val accentGroup =
    repsep(box(accent), "and") ~ tinctureRef ^^ AccentGroup.apply
    
  private lazy val alignment =
    rep(cardinal) ~ ("and" ~> cardinal) ^^ { (cs: List[Cardinal], c: Cardinal) => cs :+ c } ^^ Alignment.Enumerated.apply |
      position ^^ Alignment.Shape.apply

  private lazy val mobileChargeDescribed =
    box(mobileCharge) ~ rep(box(attitude)) ~ opt(box(alignment)) ~ opt(fill) ~ rep(accentGroup) ^^ MobileChargeDescribed.apply

  // fill is optional because mobileCharge could have an inherent color
  // or could be delayed

  private lazy val chargeDescribed =
    mobileChargeDescribed ^^ ChargeDescribed.Mobile.apply |
      ordinaryDescribed ^^ ChargeDescribed.Ordinary.apply

  private lazy val locatedCharge =
    "in" ~> opt("the") ~> location ~ count ~ chargeDescribed ^^ ChargeLocated.apply

  private lazy val surroundingCharges =
    count ~ chargeDescribed ^^ SurroundingCharges.Uniform.apply |
      locatedCharge ~ ("and" ~> locatedCharge) ^^ SurroundingCharges.Divided.apply

  private lazy val principleCharge: Parser[ChargeEtc.Principle] =
    count ~ chargeDescribed ~ opt(between ~> surroundingCharges) ^^ ChargeEtc.Principle.apply

  // TODO maybe make charge group, put everyone at the same level?
  private lazy val chargeEtc =
    "on" ~> (principleCharge: Parser[ChargeEtc.Principle]) ~ count ~ chargeDescribed ^^ ChargeEtc.Surcharge.apply |
      // TODO the "and" means that the tincture is shared
      opt("and") ~> principleCharge

  private lazy val tinctureRef =
    tincture ^^ TinctureRef.Direct.apply |
      ("of" | "as") ~> "the" ~> ordinal ^^ TinctureRef.OrdReference.apply |
      ("of" | "as") ~> "the" ~> "field" ^^^ TinctureRef.FieldReference() |
      ("of" | "as") ~> "the" ~> "last" ^^^ TinctureRef.LastReference()

  private lazy val semy =
    "semy" ~> "of" ~> chargeDescribed ^^ Semy.Of.apply |
      semyPrimitive ~ opt(tinctureRef) ^^ Semy.Primitive.apply

  private lazy val fill: Parser[Fill] =
    // variation before tincture to handle "vairy"
    variation ~ opt("of" ~> box(cardinal)) ~ tinctureRef ~ ("and" ~> tinctureRef) ^^ Fill.Variated.apply |
      tinctureRef ~ opt(semy) ^^ Fill.Flat.apply
//      "counterchanged" ^^^ Fill.Counterchanged

  private lazy val field =
    divisionLined ~ fill ~ ("and" ~> fill) ^^ Field.Divided.apply |
      fill ^^ Field.Undivided.apply

  private lazy val cantonOrChiefEtc =
    "on" ~> a ~> cantonOrChiefDescribed ~ count ~ chargeDescribed ^^ CantonOrChiefEtc.Surcharge.apply |
      // TODO the "and" means that the tincture is shared
      opt("and") ~> a ~> cantonOrChiefDescribed ^^ CantonOrChiefEtc.Principle.apply

  private lazy val overAll =
    "over" ~> "all" ~> chargeEtc
    
  // TODO charged with (for non-ordinaries)
    
  private lazy val bordureDescribed =
    "bordure" ~> opt(line) ~ fill ^^ BordureDescribed.apply
    
  private lazy val bordureEtc =
    opt("within") ~> a ~> bordureDescribed

  private lazy val blazon = {
    // field, ordinary or central charge, subordinaries or located charges or overalls?
    // TODO need over or debruised or whatever
    field ~ opt(chargeEtc) ~ rep(locatedCharge) ~ opt(cantonOrChiefEtc) ~ opt(overAll) ~ opt(bordureEtc) <~ opt(".") ^^ Blazon.apply
  }
  
  def box[A](p: Parser[A]): Parser[![A]] = {
    p ^^ !.apply
  }

  def parseString(s: String): ParseResult[Blazon] = {
    val source = StringSource(s, "input")
    // preprocess by checking that it starts with a capital
    // and that it ends with a period
    if (s.headOption.exists(_.isLower)) {
      Failure("first letter must be capitalized", Input(source, s.length))
    } else if (!s.lastOption.contains('.') || s.takeRight(2).headOption.contains('.')) {
      Failure("must end with one period", Input(source, s.length))
    } else {
      // lowercase the first letter
      val newSource = StringSource(s.take(1).toLowerCase + s.drop(1), "input")
      parseAll(blazon, newSource)
    }
  }

  private def anyOf[T](l: List[Parser[T]]): Parser[T] = {
    l.reduceLeft {
      case (acc, p) => acc | p
    }
  }

  private def upperFromReprs[T](reprs: Map[T, Set[String]]): Parser[T] = {
    val parsers = for {
      (x, rs) <- reprs.toList
      r <- rs
    } yield toTitleCase(r) ^^^ x
    anyOf(parsers)
  }

  private def toTitleCase(s: String): String = {
    s.take(1).toUpperCase + s.drop(1)
  }

  private def fromReprs[T](name: String, reprs: Map[T, Set[String]]): Parser[T] = {
    val mapping = for {
      (x, rs) <- reprs.toList
      r <- rs
    } yield (r, x)
    // sort by size reversed so we don't short circuit when one name is a prefix another
    val sorted = mapping.sortBy {
      case (string, _) => -string.length
    }
    val parsers = sorted.map {
      case (string, result) => string ^^^ result
    }
    anyOf(parsers) | failure(s"unknown $name")
  }
}

object Parser {
  
  case class Error(pos: kiama.util.Position) extends RuntimeException

  def parse(s: String)(implicit pos: Positions): Blazon = {
    new Parser(pos).parseString(s) match {
      case Success(result, next) => result
      case fail: NoSuccess => throw Error(fail.next.position)
    }
  }
}