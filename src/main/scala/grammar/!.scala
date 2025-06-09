package grammar

/**
  * Helper class to create unique identities for terminal nodes.
  */
case class ![A](value: A) extends Nonterminal {
  def ! : A = value
}
