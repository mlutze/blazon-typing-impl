package grammar

import grammar.terminal.Cardinal

enum Count extends Nonterminal {
  case A()
  case An()
  case The()
  case N(card: Cardinal)
  case AsMany()
}