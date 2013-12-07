package org.gilbertlang.glexer.token

trait SelectTokens{
    type Token
    def accept(token: Token): Boolean
}