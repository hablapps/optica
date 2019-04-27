package dev.habla
package optica
package suite

import org.scalatest._
import scalaz._, Scalaz._
import triplet._
import model._

class OrgSQLTest extends FlatSpec with Matchers {

  object OrgLogicTripletFun extends OrgLogic[λ[x => TripletFun]]
  import OrgLogicTripletFun.expertise

  val keys = ==>>("Department" -> "dpt", "Employee" -> "emp")

  "Optica" should "translate expertise into a SELECT statement" in {
    expertise("abstract").toSQL(keys).map(_.toString) shouldBe 
      \/-("""SELECT d.dpt FROM Department AS d WHERE NOT(EXISTS(SELECT e.* FROM Employee AS e WHERE (NOT(EXISTS(SELECT t.tsk FROM Task AS t WHERE ((t.tsk = "abstract") AND (e.emp = t.emp)))) AND (d.dpt = e.dpt))))""")
  }
}

