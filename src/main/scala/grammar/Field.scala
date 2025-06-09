package grammar

enum Field extends Nonterminal {
  case Divided(div: DivisionLined, fill1: Fill, fill2: Fill)
  case Undivided(fill: Fill)
}