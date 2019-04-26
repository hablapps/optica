package dev.habla.optica
package symantics
package interpreter

import triplet._

trait TripletFunGetterSym extends GetterSym[λ[x => TripletFun]] {

  def id_gt[S] = identity

  def comp_gt[S, A, B](u: TripletFun, d: TripletFun) = u andThen d

  def fork_gt[S, A, B](l: TripletFun, r: TripletFun) = 
    merge3With(l, r)(_ ++ _, _ lmerge _, _ ++ _)

  def like[S, A: Base](a: A) = first[Triplet, List[TExpr]].set(List(Like(a)))

  def not[S](b: TripletFun) = b andThen first[Triplet, List[TExpr]].modify {
    case List(e) => List(Not(e))  
    case _ => throw new Error("should never happen")
  }

  private def binary(
      x: TripletFun, y: TripletFun)(
      op: (TExpr, TExpr) => TExpr): TripletFun = 
    merge3With(x, y)(
      { case (List(e1), List(e2)) => List(op(e1, e2))
        case _ => throw new Error("should never happen") },
      _ lmerge _, _ ++ _) 

  def equal[S, A: Base](x: TripletFun, y: TripletFun) = binary(x, y)(Eq)
    
  def greaterThan[S](x: TripletFun, y: TripletFun) = binary(x, y)(GreaterThan)

  def subtract[S](x: TripletFun, y: TripletFun) = binary(x, y)(Sub)
}

trait TripletFunAffineFoldSym extends AffineFoldSym[λ[x => TripletFun]] {

  def id_af[S] = identity

  def comp_af[S, A, B](u: TripletFun, d: TripletFun) = u andThen d

  def filtered[S](p: TripletFun) = {
    case (s, f, w) => p((s, f, Set.empty)) match {
      case (e, f2, _) => (s, f2, w ++ e)
    }
  }

  def as_afl[S, A](gt: TripletFun) = gt
}

trait TripletFunFoldSym extends FoldSym[λ[x => TripletFun]] {

  def id_fl[S] = identity

  def comp_fl[S, A, B](u: TripletFun, d: TripletFun) = u andThen d

  def nonEmpty[S, A](fl: TripletFun) = {
    case (s, f, w) => 
      (List(NonEmpty(fl((s, f.map(x => (x._1, false)), Set.empty)))), f, w)
  }

  def as_fl[S, A](afl: TripletFun) = afl
}

class TripletFun extends Optica[λ[x => TripletFun]]
  with TripletFunGetterSym with TripletFunAffineFoldSym with TripletFunFoldSym

