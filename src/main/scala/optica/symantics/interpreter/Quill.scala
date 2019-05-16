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

sealed abstract class QRep[A]
case class QRepOpt[O[_, _], S, A](
  f: Quoted[Query[S] => Query[A]]) extends QRep[O[S, A]]

sealed abstract class QObs[A]
case class QGetAll[S, A](
  f: Quoted[Query[S]] => Quoted[Query[A]]) extends QObs[S => List[A]]

trait QuillGetterSym extends GetterSym[QRep, QObs] {

  def comp_gt[S, A, B](
      u: QRep[Getter[S, A]], 
      d: QRep[Getter[A, B]]) = (u, d) match {
    case (QRepOpt(f), QRepOpt(g)) => QRepOpt(quote { q: Query[S] => g(f(q)) })
  }

  def fork_gt[S, A, B](
      l: QRep[Getter[S, A]], 
      r: QRep[Getter[S, B]]) = (l, r) match {
    case (QRepOpt(f), QRepOpt(g)) => QRepOpt(quote { q: Query[S] => 
      for {
        a <- f(q)
        b <- g(q)
      } yield (a, b) 
    })
  }

  def id_gt[S] = QRepOpt(quote { q: Query[S] => q })

  def like[S, A: Base](a: A) = ???

  def not[S](b: QRep[Getter[S, Boolean]]) = b match { 
    case QRepOpt(f) => QRepOpt(quote { q: Query[S] => f(q).map(!_) })
  }

  def equal[S, A](
      x: QRep[Getter[S, A]],
      y: QRep[Getter[S, A]])(implicit
      B: Base[A]) = B match {
    case IntWitness => ??? // quote { Getter[S, Boolean](s => x.get(s) == y.get(s)) }
    case StringWitness => ??? // quote { Getter[S, Boolean](s => x.get(s) == y.get(s)) }
    case BooleanWitness => ??? // quote { Getter[S, Boolean](s => x.get(s) == y.get(s)) }
  }

  def greaterThan[S](
      x: QRep[Getter[S, Int]], 
      y: QRep[Getter[S, Int]]) = (x, y) match {
    case (QRepOpt(f), QRepOpt(g)) => QRepOpt(quote { q: Query[S] => 
      for {
        i <- f(q)
        j <- g(q)
      } yield (i > j) })
  }

  def subtract[S](
      x: QRep[Getter[S, Int]], 
      y: QRep[Getter[S, Int]]) = (x, y) match {
    case (QRepOpt(f), QRepOpt(g)) => QRepOpt(quote { q: Query[S] => 
      for {
        i <- f(q)
        j <- g(q)
      } yield (i - j) })
  }

  def get[S, A](gt: QRep[Getter[S, A]]) = ???
}

trait QuillAffineFoldSym extends AffineFoldSym[QRep, QObs] {

  def id_af[S] = ??? // Category[AffineFold].id

  def comp_af[S, A, B](
      u: QRep[AffineFold[S, A]], 
      d: QRep[AffineFold[A, B]]) = (u, d) match {
    case (QRepOpt(f), QRepOpt(g)) => QRepOpt(quote { q: Query[S] => g(f(q)) })
  }

  def filtered[S](p: QRep[Getter[S, Boolean]]) = p match {
    case QRepOpt(f) => QRepOpt(quote { q: Query[S] =>
      for {
        s <- q
        b <- f(q) if b
      } yield s
    })
  }

  def as_afl[S, A](gt: QRep[Getter[S, A]]) = gt match {
    case QRepOpt(f) => QRepOpt(f)
  }

  def getOpt[S, A](af: QRep[AffineFold[S, A]]) = ???
}

trait QuillFoldSym extends FoldSym[QRep, QObs] {

  def id_fl[S] = ??? // Category[Fold].id

  def comp_fl[S, A, B](
      u: QRep[Fold[S, A]], 
      d: QRep[Fold[A, B]]) = (u, d) match {
    case (QRepOpt(f), QRepOpt(g)) => QRepOpt(quote { q: Query[S] => g(f(q)) })
  }

  def nonEmpty[S, A](fl: QRep[Fold[S, A]]) = ??? // fl.nonEmpty

  def as_fl[S, A](afl: QRep[AffineFold[S, A]]) = afl match {
    case QRepOpt(f) => QRepOpt(f)
  }

  def getAll[S, A](fl: QRep[Fold[S, A]]) = fl match {
    case QRepOpt(f) => QGetAll[S, A](q => quote { f(q) })
  }
}

class QuillSym extends Optica[QRep, QObs]
  with QuillGetterSym with QuillAffineFoldSym with QuillFoldSym

