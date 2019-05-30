package example
package org

import optica._
import symantics.interpreter.Wrap
import symantics.Optica.tlinqOptica

import _root_.org.scalatest._
import interpreter.Nested

class TlinqTest extends FlatSpec with Matchers {

  implicit val _1 = symantics.Optica.tlinqOptica[λ[x => x]]
  implicit val _2 = Model.tlinqModel[λ[x => x]]

  object OrgLogicTlinq extends Logic[Wrap[λ[x => x], ?], λ[x => x]]
  import OrgLogicTlinq.expertise

  implicit val _3 = symantics.Optica.tlinqOptica[λ[x => Int => String]]
  implicit val _4 = Model.tlinqModel[λ[x => Int => String]]
  val _5 = implicitly[tlinq.Tlinq[λ[x => Int => String]]]

  object OrgLogicTlinqShow 
    extends Logic[Wrap[λ[x => Int => String], ?], λ[x => Int => String]]
  import OrgLogicTlinqShow.{expertise => expertiseShow}

  _5.app(expertiseShow("abstract"))(Nested[λ[x => Int => String]])(0)

  // generates ...
  //
  // (λx0 -> for (x1 <- (λx1 -> for (x2 <- (λx2 -> x2)(x1)) for (x3 <- (λx3 ->
  // (λx4 -> if ((λx5 -> !(λx6 -> exists (λx7 -> for (x8 <- (λx8 ->
  // x8.employees)(x7)) for (x9 <- (λx9 -> (λx10 -> if ((λx11 -> !(λx12 ->
  // exists (λx13 -> for (x14 <- (λx14 -> for (x15 <- (λx15 -> x15.tasks)(x14))
  // for (x16 <- (λx16 -> (λx17 -> Some((λx18 ->
  // x18.tsk)(x17)))(x16).fold(Nil)((λx17 -> yield x17)))(x15)) yield
  // x16)(x13)) for (x15 <- (λx15 -> (λx16 -> if ((λx17 -> (λx18 -> x18)(x17)
  // == (λx18 -> "abstract")(x17))(x16)) Some(x16) else
  // None)(x15).fold(Nil)((λx16 -> yield x16)))(x14)) yield
  // x15)(x12))(x11))(x10)) Some(x10) else None)(x9).fold(Nil)((λx10 -> yield
  // x10)))(x8)) yield x9)(x6))(x5))(x4)) Some(x4) else
  // None)(x3).fold(Nil)((λx4 -> yield x4)))(x2)) yield x3)(x0)) for (x2 <-
  // (λx2 -> (λx3 -> Some((λx4 -> x4.dpt)(x3)))(x2).fold(Nil)((λx3 -> yield
  // x3)))(x1)) yield x2)(for (x0 <- table_department) yield Department(x0.dpt,
  // for (x1 <- table_employee) where x0.dpt == x1.dpt yield Employee(x1.emp,
  // for (x2 <- table_task) where x1.emp == x2.emp yield Task(x2.tsk))))

  "Optica" should "translate expertise into a fold" in {
    expertise("abstract")(Nested[λ[x => x]]) shouldBe 
      List("Quality", "Research")
  }
}
