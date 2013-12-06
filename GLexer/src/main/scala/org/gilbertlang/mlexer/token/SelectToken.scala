package org.gilbertlang.mlexer.token

trait SelectTokens{
    type Token
    def accept(token: Token): Boolean
}