package dev.habla.optica
package quel

trait Quel[Repr[_]] {

  def table[A]: Repr[List[A]]

  def int(i: Int): Repr[Int]

  def bool(b: Boolean): Repr[Boolean]

  def string(s: String): Repr[String]

  def foreach[A, B](
    as: Repr[List[A]])(
    f: Repr[A] => Repr[List[B]]): Repr[List[B]]

  def where[A](c: Repr[Boolean])(as: Repr[List[A]]): Repr[List[A]]

  def yields[A](a: Repr[A]): Repr[List[A]]

  def product[A, B](a: Repr[A], b: Repr[B]): Repr[(A, B)]

  def nil[A]: Repr[List[A]]

  def subtract(x: Repr[Int], y: Repr[Int]): Repr[Int]

  def greaterThan(x: Repr[Int], y: Repr[Int]): Repr[Boolean]

  def equal[A](a1: Repr[A], a2: Repr[A]): Repr[Boolean]

  def and(p: Repr[Boolean], q: Repr[Boolean]): Repr[Boolean]

  def not(p: Repr[Boolean]): Repr[Boolean]

  def exists[A](f: Repr[List[A]]): Repr[Boolean]

  def ifs[A](b: Repr[Boolean], t: Repr[A], e: Repr[A]): Repr[A]

  def lam[A, B](f: Repr[A] => Repr[B]): Repr[A => B]

  def app[A, B](f: Repr[A => B])(a: Repr[A]): Repr[B]
}

object Quel {

  implicit object RQuel extends interpreter.R
}

