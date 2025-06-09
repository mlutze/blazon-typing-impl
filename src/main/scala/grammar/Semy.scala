package grammar

import grammar.terminal.SemyPrimitive

enum Semy extends Nonterminal {
  case Primitive(p: SemyPrimitive, tinc: Option[TinctureRef])
  case Of(ch: ChargeDescribed)
}