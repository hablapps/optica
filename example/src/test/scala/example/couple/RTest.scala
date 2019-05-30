package example
package couple

import _root_.org.scalatest._

class RTest extends FlatSpec with Matchers {

  val data: Couples = List(
    Couple(Person("Alex", 60), Person("Bert", 55)),
    Couple(Person("Cora", 33), Person("Drew", 31)),
    Couple(Person("Edna", 21), Person("Fred", 60)))

  object CoupleLogicR extends Logic[λ[x => x], λ[x => x]]
  import CoupleLogicR.differences

  "Optica" should "translate differences into a fold" in {
    differences(data) shouldBe List("Alex" -> 5, "Cora" -> 2) 
  }
}

