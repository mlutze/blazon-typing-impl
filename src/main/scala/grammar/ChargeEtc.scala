package grammar

enum ChargeEtc extends Nonterminal {
  // TODO re-stratify
  case Surcharge(bt: Principle, n: Count, ch: ChargeDescribed)
  case Principle(n: Count, ch: ChargeDescribed, surr: Option[SurroundingCharges])
}