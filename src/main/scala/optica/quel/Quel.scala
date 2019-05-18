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

  // Example (XXX: to be moved)
  
  case class CoupleRel(her: String, him: String)
  case class PersonRel(name: String, age: Int)

  trait SymSchema[Repr[_]] {

    def her(c: Repr[CoupleRel]): Repr[String]

    def him(c: Repr[CoupleRel]): Repr[String]

    def name(p: Repr[PersonRel]): Repr[String]

    def age(p: Repr[PersonRel]): Repr[Int]
  }

  // TODO: consider providing syntax for these definitions!

  def differences[Repr[_]](implicit 
      Q: Quel[Repr],
      S: SymSchema[Repr]): Repr[List[(String, Int)]] = {
    import Q._, S._
    foreach(table[CoupleRel])(c =>
    foreach(table[PersonRel])(m =>
    foreach(table[PersonRel])(w =>
    where(and(
      and(equal(her(c), name(w)), equal(him(c), name(m))), 
      greaterThan(age(w), age(m))))(
    yields(product(
      name(w), 
      subtract(age(w), age(m))))))))
  }

  case class Couple(her: Person, him: Person)
  case class Person(name: String, age: Int)

  trait NestSchema[Repr[_]] {
    
    def Person(name: Repr[String], age: Repr[Int]): Repr[Person]

    def Couple(her: Repr[Person], him: Repr[Person]): Repr[Couple]
  }

  def model[Repr[_]](implicit 
      Q: Quel[Repr],
      S: SymSchema[Repr],
      N: NestSchema[Repr]): Repr[List[Couple]] = {
    import Q._, S._
    foreach(table[CoupleRel])(c =>
    foreach(table[PersonRel])(m =>
    foreach(table[PersonRel])(w =>
    where(and(equal(her(c), name(w)), equal(him(c), name(m))))(
    yields(N.Couple(N.Person(name(w), age(w)), N.Person(name(m), age(m))))))))
  }
}

