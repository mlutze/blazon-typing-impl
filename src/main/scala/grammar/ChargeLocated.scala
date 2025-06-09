package grammar

import grammar.terminal.Location

case class ChargeLocated(loc: Location, count: Count, ch: ChargeDescribed) extends Nonterminal

