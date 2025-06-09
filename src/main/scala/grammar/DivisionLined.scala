package grammar

import grammar.terminal.{Division, Line}

case class DivisionLined(div: Division, ln: Option[Line]) extends Nonterminal
