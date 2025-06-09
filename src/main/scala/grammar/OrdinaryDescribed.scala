package grammar

import grammar.terminal.{Line, Ordinary}

case class OrdinaryDescribed(ord: Ordinary, ln: Option[Line], fill: Option[Fill]) extends Nonterminal
