package org.lancegatlin

trait Printable[A] {
  def print(a: A) : String
}

class SimplePrintable[A] extends Printable[A] {
  def print(a: A) = a.toString
}