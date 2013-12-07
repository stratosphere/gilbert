package org.gilbertlang.mlibrary

import MTypes.Helper._
import MTypes._
import MValues.Helper._


object ConvenienceMethods {
  def newTVV() = {
    (newTV(),newVV(),newVV())
  }
  
  def newNTVV() = {
    (newNumericTV(),newVV(),newVV())
  }
  
  def newUNTVV() = {
    (untv, uvv, uvv)
  }
  
  def newUTVV() = {
    (utv,uvv,uvv)
  }
}