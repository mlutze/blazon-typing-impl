package grammar

enum CantonOrChiefEtc extends Nonterminal {
  // TODO re-stratify
  case Surcharge(c: CantonOrChiefDescribed, n: Count, ch: ChargeDescribed)
  case Principle(c: CantonOrChiefDescribed)
}
