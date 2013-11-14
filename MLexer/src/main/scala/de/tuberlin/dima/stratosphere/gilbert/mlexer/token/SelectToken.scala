package de.tuberlin.dima.stratosphere.gilbert.mlexer.token

trait SelectTokens{
    type Token
    def accept(token: Token): Boolean
}