package optica
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
case class QGetter[S, A](f: Quoted[S => A]) extends QRep[Getter[S, A]]
case class QAffine[S, A](f: Quoted[Query[S] => Query[A]]) 
  extends QRep[AffineFold[S, A]]
case class QFold[S, A](f: Quoted[Query[S] => Query[A]]) 
  extends QRep[Fold[S, A]]

sealed abstract class QObs[A]
case class QGetAll[S, A](
  f: Quoted[Query[S]] => Quoted[Query[A]]) extends QObs[S => List[A]]

trait QuillGetterSym extends GetterSym[QRep, QObs] {

  def comp_gt[S, A, B](
      u: QRep[Getter[S, A]], 
      d: QRep[Getter[A, B]]) = (u, d) match {
    case (QGetter(f), QGetter(g)) => QGetter(quote { s: S => g(f(s)) })
  }

  def fork_gt[S, A, B](
      l: QRep[Getter[S, A]], 
      r: QRep[Getter[S, B]]) = (l, r) match {
    case (QGetter(f), QGetter(g)) => QGetter(quote { s: S => (f(s), g(s)) })
  }

  def id_gt[S] = QGetter(quote { s: S => s })

  def like[S, A](a: A)(implicit B: Base[A]) = B match {
    case IntWitness => QGetter[S, Int](quote { _: S => a })
    case StringWitness => QGetter[S, String](quote { _: S => a })
    case BooleanWitness => QGetter[S, Boolean](quote { _: S => a })
  }

  def not[S](b: QRep[Getter[S, Boolean]]) = b match {
    case QGetter(f) => QGetter(quote { s: S => !f(s) })
  }

  def equal[S, A](
      x: QRep[Getter[S, A]],
      y: QRep[Getter[S, A]])(implicit
      B: Base[A]) = {
    val QGetter(f) = x
    val QGetter(g) = y
    B match {
      case IntWitness => QGetter(quote { s: S => f(s) == g(s) })
      case StringWitness => QGetter(quote { (s: S) => f(s) == g(s) })
      case BooleanWitness => QGetter(quote { (s: S) => f(s) == g(s) })
    }
  }

  def greaterThan[S](
      x: QRep[Getter[S, Int]], 
      y: QRep[Getter[S, Int]]) = (x, y) match {
    case (QGetter(f), QGetter(g)) => QGetter(quote { s: S => f(s) > g(s) })
  }

  def subtract[S](
      x: QRep[Getter[S, Int]], 
      y: QRep[Getter[S, Int]]) = (x, y) match {
    case (QGetter(f), QGetter(g)) => QGetter(quote { s: S => f(s) - g(s) })
  }

  def get[S, A](gt: QRep[Getter[S, A]]) = ???
}

trait QuillAffineFoldSym extends AffineFoldSym[QRep, QObs] {

  def id_af[S] = QAffine(quote { q: Query[S] => q })

  def comp_af[S, A, B](
      u: QRep[AffineFold[S, A]], 
      d: QRep[AffineFold[A, B]]) = (u, d) match {
    case (QAffine(f), QAffine(g)) => QAffine(quote { q: Query[S] => 
      g(f(q))
    })
  }

  def filtered[S](p: QRep[Getter[S, Boolean]]) = p match {
    case QGetter(f) => QAffine(quote { q: Query[S] =>
      for {
        s <- q if f(s)
      } yield s
    })
  }

  def as_afl[S, A](gt: QRep[Getter[S, A]]) = gt match {
    case QGetter(f) => QAffine(quote { q: Query[S] =>
      for {
        s <- q
      } yield f(s)
    })
  }

  def getOpt[S, A](af: QRep[AffineFold[S, A]]) = ???
}

trait QuillFoldSym extends FoldSym[QRep, QObs] {

  def id_fl[S] = QFold(quote { q: Query[S] => q })

  def comp_fl[S, A, B](
      u: QRep[Fold[S, A]], 
      d: QRep[Fold[A, B]]) = (u, d) match {
    case (QFold(f), QFold(g)) => QFold(quote { q: Query[S] => 
      g(f(q))
    })
  }

  def nonEmpty[S, A](fl: QRep[Fold[S, A]]) = fl match {
    case QFold(f) => QGetter(quote { s: S => 
      // XXX: extremely ugly workaround to do monadic `pure` for `Query`
      f(query[Empty].map(_ => s)).size > 0 
    })
  }
  case class Empty()

  def as_fl[S, A](afl: QRep[AffineFold[S, A]]) = afl match {
    case QAffine(f) => QFold(f)
  }

  def getAll[S, A](fl: QRep[Fold[S, A]]) = fl match {
    case QFold(f) => QGetAll[S, A](s => quote { f(s) })
  }
}

class QuillSym extends Optica[QRep, QObs]
  with QuillGetterSym with QuillAffineFoldSym with QuillFoldSym

