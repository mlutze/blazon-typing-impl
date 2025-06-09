package grammar

import grammar.terminal.Location

enum SurroundingCharges extends Nonterminal {
  case Uniform(count: Count, ch: ChargeDescribed)
  case Divided(ch1: ChargeLocated, ch2: ChargeLocated)
}
