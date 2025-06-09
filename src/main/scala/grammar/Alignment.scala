package grammar

import grammar.terminal.{Cardinal, Position}

enum Alignment extends Nonterminal {
  case Enumerated(cards: List[Cardinal])
  case Shape(pos: Position)
}

object Alignment {
  def canDisplay(al: Alignment, n: Int): Boolean = al match {
    case Enumerated(cards) => cards.map(Cardinal.value).sum == n
    case Shape(pos) => Position.canDisplay(pos, n) && n != Integer.MAX_VALUE
  }
}