package grammar

import grammar.terminal.Line

case class BordureDescribed(ln: Option[Line], fill: Fill) extends Nonterminal
