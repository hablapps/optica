package dev.habla
package optica
package suite

import org.scalatest._
import example._
import model._

class OrgRTest extends FlatSpec with Matchers {

  val data: Org = List(
    Department("Product", List(
      Employee("Alex", List(Task("build"))),
      Employee("Bert", List(Task("build"))))),
    Department("Quality", List.empty),
    Department("Research", List(
      Employee("Cora", List(Task("abstract"), Task("build"), Task("design"))),
      Employee("Drew", List(Task("abstract"), Task("design"))),
      Employee("Edna", List(Task("abstract"), Task("call"), Task("design"))))),
    Department("Sales", List(
      Employee("Fred", List(Task("call"))))))

  object OrgLogicR extends OrgLogic[λ[x => x]]
  import OrgLogicR.expertise

  "Optica" should "translate expertise into a fold" in {
    expertise("abstract").getAll(data) shouldBe List("Quality", "Research")
  }
}

