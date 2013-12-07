package org.gilbertlang.glexer.token

trait DiscardWhitespaces extends SelectTokens with GTokens {
  abstract override def accept(token: Token) = {
    token match {
      case Whitespace(_) => false
      case _ => super.accept(token)
    }
  }
}