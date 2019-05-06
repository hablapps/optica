package dev.habla.optica
package symantics
package interpreter

import scalaz._, Scalaz._
import concrete._
import CategoryWithProduct.syntax._, Getter.syntax._, Fold.syntax._
import Base._

trait RGetterSym extends GetterSym[λ[x => x], λ[x => x]] {

  def comp_gt[S, A, B](u: Getter[S, A], d: Getter[A, B]) = u >>> d

  def fork_gt[S, A, B](l: Getter[S, A], r: Getter[S, B]) = l *** r

  def id_gt[S] = Category[Getter].id

  def like[S, A: Base](a: A) = a

  def not[S](b: Getter[S, Boolean]) = Getter.not(b)

  def equal[S, A](
      x: Getter[S, A],
      y: Getter[S, A])(implicit
      B: Base[A]) = B match {
    case IntWitness => x === y
    case StringWitness => x === y
    case BooleanWitness => x === y
  }

  def greaterThan[S](x: Getter[S, Int], y: Getter[S, Int]) = x > y

  def subtract[S](x: Getter[S, Int], y: Getter[S, Int]) = x - y

  def get[S, A](gt: Getter[S, A]) = gt.get
}

trait RAffineFoldSym extends AffineFoldSym[λ[x => x], λ[x => x]] {

  def id_af[S] = Category[AffineFold].id

  def comp_af[S, A, B](u: AffineFold[S, A], d: AffineFold[A, B]) = u >>> d

  def filtered[S](p: Getter[S, Boolean]) = AffineFold.filtered(p)

  def as_afl[S, A](gt: Getter[S, A]) = gt

  def getOpt[S, A](af: AffineFold[S, A]) = af.getOpt
}

trait RFoldSym extends FoldSym[λ[x => x], λ[x => x]] {

  def id_fl[S] = Category[Fold].id

  def comp_fl[S, A, B](u: Fold[S, A], d: Fold[A, B]) = u >>> d

  def nonEmpty[S, A](fl: Fold[S, A]) = fl.nonEmpty

  def as_fl[S, A](afl: AffineFold[S, A]) = afl

  def getAll[S, A](fl: Fold[S, A]) = fl.getAll
}

class R extends Optica[λ[x => x], λ[x => x]]
  with RGetterSym with RAffineFoldSym with RFoldSym

