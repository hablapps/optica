package dev.habla.optica
package symantics
package interpreter

import scalaz._, Scalaz._
import concrete._
import CategoryWithProduct.syntax._, Getter.syntax._, Fold.syntax._
import Base._
import io.getquill._

object ctx extends SqlMirrorContext(MirrorSqlDialect, Literal)
import ctx._

sealed abstract class QObs[A]
case class QGetAll[S, A](f: Quoted[S] => Quoted[Query[A]]) 
    extends QObs[S => List[A]]

trait QuillGetterSym extends GetterSym[Quoted, QObs] {

  def comp_gt[S, A, B](u: Quoted[Getter[S, A]], d: Quoted[Getter[A, B]]) = 
    quote { Getter[S, B](s => d.get(u.get(s))) }

  def fork_gt[S, A, B](l: Getter[S, A], r: Getter[S, B]) =
    quote { Getter[S, (A, B)](s => (l.get(s), r.get(s))) }

  def id_gt[S] = quote { Getter[S, S](s => s) }

  def like[S, A: Base](a: A) = quote { Getter[S, A](_ => a) }

  def not[S](b: Getter[S, Boolean]) = 
    quote { Getter[S, Boolean](s => ! b.get(s)) }

  def equal[S, A](
      x: Getter[S, A],
      y: Getter[S, A])(implicit
      B: Base[A]) = B match {
    case IntWitness => quote { Getter[S, Boolean](s => x.get(s) == y.get(s)) }
    case StringWitness => quote { Getter[S, Boolean](s => x.get(s) == y.get(s)) }
    case BooleanWitness => quote { Getter[S, Boolean](s => x.get(s) == y.get(s)) }
  }

  def greaterThan[S](x: Getter[S, Int], y: Getter[S, Int]) =
    quote { Getter[S, Boolean](s => x.get(s) > y.get(s)) }

  def subtract[S](x: Getter[S, Int], y: Getter[S, Int]) =
    quote { Getter[S, Int](s => x.get(s) - y.get(s)) }

  def get[S, A](gt: Getter[S, A]) = ???
}

trait QuillAffineFoldSym extends AffineFoldSym[Quoted, QObs] {

  def id_af[S] = ??? // Category[AffineFold].id

  def comp_af[S, A, B](u: AffineFold[S, A], d: AffineFold[A, B]) = ??? // u >>> d

  def filtered[S](p: Getter[S, Boolean]) = ??? // AffineFold.filtered(p)

  def as_afl[S, A](gt: Getter[S, A]) = ??? // gt

  def getOpt[S, A](af: AffineFold[S, A]) = ???
}

trait QuillFoldSym extends FoldSym[Quoted, QObs] {

  def id_fl[S] = ??? // Category[Fold].id

  def comp_fl[S, A, B](u: Fold[S, A], d: Fold[A, B]) = ??? // u >>> d

  def nonEmpty[S, A](fl: Fold[S, A]) = ??? // fl.nonEmpty

  def as_fl[S, A](afl: AffineFold[S, A]) = afl

  def getAll[S, A](fl: Fold[S, A]) =
    QGetAll[S, A](q => quote { fl.getAll(q) })
}

class QuillSym extends Optica[Quoted, QObs]
  with QuillGetterSym with QuillAffineFoldSym with QuillFoldSym

