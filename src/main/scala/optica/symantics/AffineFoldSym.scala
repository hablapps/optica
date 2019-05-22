package optica
package symantics

import concrete._

trait AffineFoldSym[Repr[_], Obs[_]] {

  def id_af[S]: Repr[AffineFold[S, S]]

  def comp_af[S, A, B](
    u: Repr[AffineFold[S, A]],
    d: Repr[AffineFold[A, B]]): Repr[AffineFold[S, B]]

  def filtered[S](p: Repr[Getter[S, Boolean]]): Repr[AffineFold[S, S]]

  def as_afl[S, A](gt: Repr[Getter[S, A]]): Repr[AffineFold[S, A]]

  def getOpt[S, A](af: Repr[AffineFold[S, A]]): Obs[S => Option[A]]
}

object AffineFoldSym {

  trait Syntax {

    implicit class AffineFoldOps[Repr[_], Obs[_], S, A](
        af: Repr[AffineFold[S, A]])(implicit 
        ev: AffineFoldSym[Repr, Obs]) {
          
      def >>>[B](other: Repr[AffineFold[A, B]]): Repr[AffineFold[S, B]] =
        ev.comp_af(af, other)

      def getOpt: Obs[S => Option[A]] = ev.getOpt(af)
    }

    implicit def gt_as_afl[Repr[_], Obs[_], S, A](
        af: Repr[Getter[S, A]])(implicit
        ev: AffineFoldSym[Repr, Obs]): Repr[AffineFold[S, A]] =
      ev.as_afl(af)
  }

  object syntax extends Syntax
}

