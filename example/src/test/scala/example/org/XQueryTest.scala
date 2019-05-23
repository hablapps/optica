package example
package org

import _root_.org.scalatest._
import optica._
import xquery._

class XQueryTest extends FlatSpec with Matchers {

  object OrgLogicXQuery extends Logic[λ[x => XQuery], λ[x => XQuery]]
  import OrgLogicXQuery.expertise

  "Optica" should "translate differences into an XQuery expression" in {
    expertise("abstract").toString shouldBe """/xml/department[not(exists(employee[not(exists(task/tsk[. = "abstract"]))]))]/dpt"""
  }
}

