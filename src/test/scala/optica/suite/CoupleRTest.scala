package dev.habla
package optica
package suite

import org.scalatest._
import example._
import model._

class CoupleRTest extends FlatSpec with Matchers {

  val data: Couples = List(
    Couple(Person("Alex", 60), Person("Bert", 55)),
    Couple(Person("Cora", 33), Person("Drew", 31)),
    Couple(Person("Edna", 21), Person("Fred", 60)))

  object CoupleLogicR extends CoupleLogic[λ[x => x]]
  import CoupleLogicR.differences

  "Optica" should "recover differences" in {
    differences.getAll(data) shouldBe List("Alex" -> 5, "Cora" -> 2) 
  }
}

