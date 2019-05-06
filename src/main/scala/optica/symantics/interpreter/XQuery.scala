package dev.habla.optica
package symantics
package interpreter

import xquery._
import Base._

trait XQueryGetterSym extends GetterSym[λ[x => XQuery]] {

  def id_gt[S] = Self

  def comp_gt[S, A, B](u: XQuery, d: XQuery) = Seq(u, d)

  def fork_gt[S, A, B](l: XQuery, r: XQuery) = Tuple(l, r)

  def like[S, A](a: A)(implicit B: Base[A]) = B match {
    case IntWitness => PInt(a)
    case StringWitness => PString(a)
    case BooleanWitness => PBool(a)
  }

  def not[S](b: XQuery) = Func("not", b)

  def equal[S, A: Base](x: XQuery, y: XQuery) = Oper("=", x, y)

  def greaterThan[S](x: XQuery, y: XQuery) = Oper(">", x, y)

  def subtract[S](x: XQuery, y: XQuery) = Oper("-", x, y)
}

trait XQueryAffineFoldSym extends AffineFoldSym[λ[x => XQuery]] {

  def id_af[S] = Self
  
  def comp_af[S, A, B](u: XQuery, d: XQuery) = Seq(u, d)

  def filtered[S](p: XQuery) = Filter(p)

  def as_afl[S, A](gt: XQuery) = gt
}

trait XQueryFoldSym extends FoldSym[λ[x => XQuery], λ[x => XQuery]] {

  def id_fl[S] = Self

  def comp_fl[S, A, B](u: XQuery, d: XQuery) = Seq(u, d)

  def nonEmpty[S, A](fl: XQuery) = Func("exists", fl)

  def as_fl[S, A](afl: XQuery) = afl

  def getAll[S, A](fl: XQuery) = Seq(Document, Seq(Name("xml"), fl))
}

class XQuerySym extends Optica[λ[x => XQuery], λ[x => XQuery]]
  with XQueryGetterSym with XQueryAffineFoldSym with XQueryFoldSym

