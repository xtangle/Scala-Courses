package week4

import idealized.scala._

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = True
  def predecessor: Nat = throw new NoSuchElementException("Zero.predecessor")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = that.isZero.ifThenElse(Zero, throw new IllegalArgumentException("Negative number"))
}


class Succ(n: Nat) extends Nat {
  def isZero: Boolean = False
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = that.isZero.ifThenElse(this, n - that.predecessor)
}