package grammar

import grammar.terminal.{Cardinal, Variation}

enum Fill extends Nonterminal {
  case Flat(tinc: TinctureRef, semy: Option[Semy])
  case Variated(va: Variation, card: Option[![Cardinal]], tinc1: TinctureRef, tinc2: TinctureRef)
}