package dev.habla
package optica
package suite

import org.scalatest._
import xquery._
import model._

class OrgXQueryTest extends FlatSpec with Matchers {

  object OrgLogicXQuery extends OrgLogic[λ[x => XQuery]]
  import OrgLogicXQuery.expertise

  "Optica" should "translate differences into a fold" in {
    expertise("abstract").toString shouldBe """department[not(exists(employee[not(exists(task/tsk[. = "abstract"]))]))]/dpt"""
  }
}

