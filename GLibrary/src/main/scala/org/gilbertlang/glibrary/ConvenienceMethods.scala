package org.gilbertlang.glibrary

import Types.Helper._
import Types._
import Values.Helper._


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