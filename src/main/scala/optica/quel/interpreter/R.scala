package dev.habla.optica
package quel
package interpreter

trait RQuel extends Quel[λ[x => x]] {

  def table[A]: List[A] = ???

  def int(i: Int) = i

  def bool(b: Boolean) = b

  def string(s: String) = s

  def foreach[A, B](as: List[A])(f: A => List[B]): List[B] =
    as.flatMap(f)

  def where[A](c: Boolean)(as: List[A]): List[A] =
    if (c) as else nil

  def yields[A](a: A): List[A] = List(a)

  def product[A, B](a: A, b: B) = (a, b)

  def nil[A]: List[A] = List()

  def subtract(x: Int, y: Int) = x - y

  def greaterThan(x: Int, y: Int) = x > y

  def equal[A](a1: A, a2: A) = a1 == a2

  def and(p: Boolean, q: Boolean) = p && q

  def not(b: Boolean) = !b

  def exists[A](as: List[A]) = as.nonEmpty

  def lam[A, B](f: A => B) = f

  def app[A, B](f: A => B)(a: A) = f(a)
}

class R extends RQuel

