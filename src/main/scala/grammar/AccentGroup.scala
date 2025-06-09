package grammar

import grammar.terminal.Accent

case class AccentGroup(accs: List[![Accent]], tinc: TinctureRef) extends Nonterminal
