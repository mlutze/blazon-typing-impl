package grammar

import grammar.terminal.{Attitude, MobileCharge}

case class MobileChargeDescribed(ch: ![MobileCharge], atts: List[![Attitude]], pos: Option[![Alignment]], fill: Option[Fill], accs: List[AccentGroup]) extends Nonterminal
