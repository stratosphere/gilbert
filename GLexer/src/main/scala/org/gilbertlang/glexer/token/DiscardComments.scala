package org.gilbertlang.glexer.token

trait DiscardComments extends SelectTokens with GTokens {
  abstract override def accept(token: Token) = {
    token match{
      case _: Comment => false
      case _ => super.accept(token)
    }
  }
}