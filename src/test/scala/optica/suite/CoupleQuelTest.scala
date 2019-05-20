package dev.habla
package optica
package suite

import symantics.interpreter.Down
import symantics.Optica.quelOptica

import org.scalatest._
import model._

class CoupleQuelTest extends FlatSpec with Matchers {

  implicit val _1 = symantics.Optica.quelOptica[λ[x => x]]
  implicit val _2 = CoupleModel.quelCoupleModel[λ[x => x]]

  object CoupleLogicQuel extends CoupleLogic[Down[λ[x => x], ?], λ[x => x]]
  import CoupleLogicQuel.differences

  "Optica" should "translate differences into a fold" in {
    differences(CoupleModel.model[λ[x => x]]) shouldBe List("Alex" -> 5, "Cora" -> 2) 
  }
}

