package dev.habla.optica
package symantics
package interpreter

import quel._
import concrete._
import Base._

sealed abstract class Down[Repr[_], A]
case class DownGetter[Repr[_], S, A](f: Repr[S => A]) 
  extends Down[Repr, Getter[S, A]]
case class DownAffine[Repr[_], S, A](f: Repr[S => Option[A]]) 
  extends Down[Repr, AffineFold[S, A]]
case class DownFold[Repr[_], S, A](f: Repr[S => List[A]]) 
  extends Down[Repr, Fold[S, A]]

class QuelSym[Repr[_]](implicit Q: Quel[Repr]) 
    extends Optica[Down[Repr, ?], Repr] {
  import Q._

  /* Getter */

  def comp_gt[S, A, B](
      u: Down[Repr, Getter[S, A]],
      d: Down[Repr, Getter[A, B]]) = (u, d) match {
    case (DownGetter(f), DownGetter(g)) => 
      DownGetter(lam(s => app(g)(app(f)(s))))
  }

  def fork_gt[S, A, B](
      l: Down[Repr, Getter[S, A]],
      r: Down[Repr, Getter[S, B]]) = (l, r) match {
    case (DownGetter(f), DownGetter(g)) =>
      DownGetter(lam(s => product(app(f)(s), app(g)(s))))
  }

  def id_gt[S] = DownGetter(lam(identity))

  def like[S, A](a: A)(implicit B: Base[A]) = DownGetter(lam[S, A](_ =>
    B match {
      case IntWitness => int(a)
      case BooleanWitness => bool(a)
      case StringWitness => string(a)
    }))

  def not[S](b: Down[Repr, Getter[S, Boolean]]) = b match {
    case DownGetter(f) => DownGetter(lam(s => Q.not(app(f)(s))))
  }

  def equal[S, A: Base](
      x: Down[Repr, Getter[S, A]],
      y: Down[Repr, Getter[S, A]]) = (x, y) match {
    case (DownGetter(f), DownGetter(g)) =>
      DownGetter(lam(s => Q.equal(app(f)(s), app(g)(s))))
  }

  def greaterThan[S](
      x: Down[Repr, Getter[S, Int]], 
      y: Down[Repr, Getter[S, Int]]) = (x, y) match {
    case (DownGetter(f), DownGetter(g)) =>
      DownGetter(lam(s => Q.greaterThan(app(f)(s), app(g)(s))))
  }

  def subtract[S](
      x: Down[Repr, Getter[S, Int]], 
      y: Down[Repr, Getter[S, Int]]) = (x, y) match {
    case (DownGetter(f), DownGetter(g)) =>
      DownGetter(lam(s => Q.subtract(app(f)(s), app(g)(s))))
  }

  def get[S, A](gt: Down[Repr, Getter[S, A]]) = gt match {
    case DownGetter(f) => f
  }

  /* Affine Fold */

 def id_af[S] = DownAffine(lam(some))

  def comp_af[S, A, B](
      u: Down[Repr, AffineFold[S, A]], 
      d: Down[Repr, AffineFold[A, B]]) = (u, d) match {
    case (DownAffine(f), DownAffine(g)) =>
      DownAffine(lam(s => ofold(app(f)(s))(none, lam(app(g)))))
  }

  def filtered[S](p: Down[Repr, Getter[S, Boolean]]) = p match {
    case DownGetter(f) => DownAffine(lam(s => ifs(app(f)(s), some(s), none)))
  }

  def as_afl[S, A](gt: Down[Repr, Getter[S, A]]) = gt match {
    case DownGetter(f) => DownAffine(lam(s => some(app(f)(s))))
  }

  // Ouch, this is what we sacrifice by using the current `DownAffine`!
  def getOpt[S, A](af: Down[Repr, AffineFold[S, A]]) = af match {
    case DownAffine(f) => f
  }

  /* Fold */

 def id_fl[S] = DownFold(lam(yields))

  def comp_fl[S, A, B](
      u: Down[Repr, Fold[S, A]], 
      d: Down[Repr, Fold[A, B]]) = (u, d) match {
    case (DownFold(f), DownFold(g)) =>
      DownFold(lam(s => 
        foreach(app(f)(s))(a =>
          foreach(app(g)(a))(yields))))
  }

  def nonEmpty[S, A](fl: Down[Repr, Fold[S, A]]) = fl match {
    case DownFold(f) => DownGetter(lam(s => exists(app(f)(s))))
  }

  def as_fl[S, A](afl: Down[Repr, AffineFold[S, A]]) = afl match {
    case DownAffine(f) => 
      DownFold(lam(s => ofold(app(f)(s))(nil, lam(yields))))
  }

  def getAll[S, A](fl: Down[Repr, Fold[S, A]]) = fl match {
    case DownFold(f) => f
  }
}

