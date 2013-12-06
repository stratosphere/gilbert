package org.gilbertlang.input

import scala.util.parsing.input.Reader

object EOFReader{
  val EofCh = '\032';
  
  def apply[T](reader: Reader[T]) = new EOFReader(reader)
}

class EOFReader[T](val reader: Reader[T], val eofEmitted:Boolean = false) extends Reader[T]{
  import EOFReader.EofCh
  
  def first = reader.first
  def rest = new EOFReader(reader.rest,first == EofCh)
  
  def atEnd = reader.atEnd && eofEmitted
  
  def pos = reader.pos
}