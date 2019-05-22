package example
package test

import _root_.org.scalatest._
import scalaz._, Scalaz._

import optica._
import sql._
import triplet._
import couple._

class CoupleSQLTest extends FlatSpec with Matchers {

  type Obs[_] = TypeNme ==>> FieldNme => Error \/ SSelect

  object CoupleLogicTripletFun extends Logic[λ[x => TripletFun], Obs]
  import CoupleLogicTripletFun.differences

  "Optica" should "translate difference into a SELECT statement" in {
    differences(==>>("Person" -> "name")).map(_.toString) shouldBe 
      \/-("SELECT w.name, (w.age - m.age) FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name INNER JOIN Person AS m ON c.him = m.name WHERE (w.age > m.age)")
  }
}

