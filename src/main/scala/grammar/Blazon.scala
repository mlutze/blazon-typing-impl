package grammar

case class Blazon(field: Field, ch: Option[ChargeEtc], more: List[ChargeLocated], cant: Option[CantonOrChiefEtc], over: Option[ChargeEtc], bord: Option[BordureDescribed]) extends Nonterminal
