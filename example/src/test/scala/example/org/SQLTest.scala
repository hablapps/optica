package example
package org

import _root_.org.scalatest._
import scalaz._, Scalaz._

import optica._
import sql._
import triplet._

class SQLTest extends FlatSpec with Matchers {

  type Obs[_] = TypeNme ==>> FieldNme => Error \/ SSelect

  object OrgLogicTripletFun extends Logic[λ[x => TripletFun], Obs]
  import OrgLogicTripletFun.expertise

  val keys = ==>>("Department" -> "dpt", "Employee" -> "emp")

  "Optica" should "translate expertise into a SELECT statement" in {
    expertise("abstract")(keys).map(_.toString) shouldBe 
      \/-("""SELECT d.dpt FROM Department AS d WHERE NOT(EXISTS(SELECT e.* FROM Employee AS e WHERE (NOT(EXISTS(SELECT t.tsk FROM Task AS t WHERE ((t.tsk = "abstract") AND (e.emp = t.emp)))) AND (d.dpt = e.dpt))))""")
  }
}

