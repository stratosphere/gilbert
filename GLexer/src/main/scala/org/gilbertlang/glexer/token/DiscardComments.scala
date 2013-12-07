package org.gilbertlang.mlexer.token

trait DiscardComments extends SelectTokens with MTokens {
  abstract override def accept(token: Token) = {
    token match{
      case _: Comment => false
      case _ => super.accept(token)
    }
  }
}