package grammar

import grammar.terminal.{Ordinal, Tincture}

enum TinctureRef extends Nonterminal {
  case Direct(tinc: Tincture)
  case OrdReference(ord: Ordinal)
  case LastReference()
  case FieldReference()
}