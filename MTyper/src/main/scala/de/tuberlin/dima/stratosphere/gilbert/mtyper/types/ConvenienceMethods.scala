package de.tuberlin.dima.stratosphere.gilbert.mtyper.types

import MTypes.Helper._
import MValues.Helper._

object ConvenienceMethods {
  def newTVV() = {
    (newTV(),newVV(),newVV())
  }
  
  def newNTVV() = {
    (newNumericTV(),newVV(),newVV())
  }

}