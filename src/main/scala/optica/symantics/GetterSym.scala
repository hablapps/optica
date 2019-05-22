package optica
package symantics

import concrete.Getter

trait GetterSym[Repr[_], Obs[_]] {

  def id_gt[S]: Repr[Getter[S, S]]

  def comp_gt[S, A, B](
    u: Repr[Getter[S, A]],
    d: Repr[Getter[A, B]]): Repr[Getter[S, B]]

  def fork_gt[S, A, B](
    l: Repr[Getter[S, A]],
    r: Repr[Getter[S, B]]): Repr[Getter[S, (A, B)]]

  def like[S, A: Base](a: A): Repr[Getter[S, A]]

  def not[S](b: Repr[Getter[S, Boolean]]): Repr[Getter[S, Boolean]]

  def equal[S, A: Base](
    x: Repr[Getter[S, A]],
    y: Repr[Getter[S, A]]): Repr[Getter[S, Boolean]]

  def greaterThan[S](
    x: Repr[Getter[S, Int]],
    y: Repr[Getter[S, Int]]): Repr[Getter[S, Boolean]]

  def subtract[S](
    x: Repr[Getter[S, Int]],
    y: Repr[Getter[S, Int]]): Repr[Getter[S, Int]]

  def get[S, A](gt: Repr[Getter[S, A]]): Obs[S => A]
}

object GetterSym {

  trait Syntax {

    implicit class GetterOps[Repr[_], Obs[_], S, A](
        gt: Repr[Getter[S, A]])(implicit
        ev: GetterSym[Repr, Obs]) {

      def >>>[B](other: Repr[Getter[A, B]]): Repr[Getter[S, B]] =
        ev.comp_gt(gt, other)

      def ***[B](other: Repr[Getter[S, B]]): Repr[Getter[S, (A, B)]] =
        ev.fork_gt(gt, other)

      def get: Obs[S => A] = ev.get(gt)
    }

    implicit class GetterBaseOps[Repr[_], Obs[_], S, B: Base](
        gt: Repr[Getter[S, B]])(implicit
        ev: GetterSym[Repr, Obs]) {

      def ===(other: Repr[Getter[S, B]]): Repr[Getter[S, Boolean]] =
        ev.equal(gt, other)
    }

    implicit class GetterArithOps[Repr[_], Obs[_], S](
        gt: Repr[Getter[S, Int]])(implicit
        ev: GetterSym[Repr, Obs]) {

      def >(y: Repr[Getter[S, Int]]): Repr[Getter[S, Boolean]] =
        ev.greaterThan(gt, y)

      def -(y: Repr[Getter[S, Int]]): Repr[Getter[S, Int]] =
        ev.subtract(gt, y)
    }

    implicit def liftLike[Repr[_], Obs[_], S, B: Base](
        b: B)(implicit
        ev: GetterSym[Repr, Obs]): Repr[Getter[S, B]] =
      ev.like(b)
  }

  object syntax extends Syntax
} 

