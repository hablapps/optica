package dev.habla
package optica
package suite

import symantics.interpreter.Down
import symantics.Optica.quelOptica

import org.scalatest._
import model._

class OrgQuelTest extends FlatSpec with Matchers {

  implicit val _1 = symantics.Optica.quelOptica[λ[x => x]]
  implicit val _2 = OrgModel.quelOrgModel[λ[x => x]]

  object OrgLogicQuel extends OrgLogic[Down[λ[x => x], ?], λ[x => x]]
  import OrgLogicQuel.expertise

  "Optica" should "translate differences into a fold" in {
    expertise("abstract")(OrgModel.model[λ[x => x]]) shouldBe 
      List("Quality", "Research")
  }
}

