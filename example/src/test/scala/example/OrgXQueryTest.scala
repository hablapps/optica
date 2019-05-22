package example
package test

import _root_.org.scalatest._
import optica._
import xquery._
import org._

class OrgXQueryTest extends FlatSpec with Matchers {

  object OrgLogicXQuery extends Logic[λ[x => XQuery], λ[x => XQuery]]
  import OrgLogicXQuery.expertise

  "Optica" should "translate differences into an XQuery expression" in {
    expertise("abstract").toString shouldBe """/xml/department[not(exists(employee[not(exists(task/tsk[. = "abstract"]))]))]/dpt"""
  }
}

