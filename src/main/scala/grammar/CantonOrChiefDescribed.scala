package grammar

import grammar.terminal.{CantonOrChief, Line}

case class CantonOrChiefDescribed(c: CantonOrChief, ln: Option[Line], fill: Option[Fill]) extends Nonterminal
