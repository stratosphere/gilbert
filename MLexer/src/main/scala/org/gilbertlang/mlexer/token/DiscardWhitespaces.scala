package de.tuberlin.dima.stratosphere.gilbert.mlexer.token

trait DiscardWhitespaces extends SelectTokens with MTokens {
  abstract override def accept(token: Token) = {
    token match {
      case Whitespace(_) => false
      case _ => true
    }
  }
}