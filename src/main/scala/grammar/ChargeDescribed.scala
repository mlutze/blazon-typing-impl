package grammar

enum ChargeDescribed extends Nonterminal {
  case Ordinary(ord: grammar.OrdinaryDescribed)
  case Mobile(ch: grammar.MobileChargeDescribed)
}