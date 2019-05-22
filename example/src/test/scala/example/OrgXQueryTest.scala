package example
package suite

import org.scalatest._
import xquery._
import model._

class OrgXQueryTest extends FlatSpec with Matchers {

  object OrgLogicXQuery extends OrgLogic[λ[x => XQuery], λ[x => XQuery]]
  import OrgLogicXQuery.expertise

  "Optica" should "translate differences into an XQuery expression" in {
    expertise("abstract").toString shouldBe """/xml/department[not(exists(employee[not(exists(task/tsk[. = "abstract"]))]))]/dpt"""
  }
}
