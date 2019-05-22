package example
package test

import _root_.org.scalatest._
import cats.effect.IO
import doobie._
import doobie.implicits._
import example.couple._
import optica.sql._
import optica.triplet._
import scalaz.Scalaz._
import scalaz._

class CoupleSQLTest extends FlatSpec with Matchers {

  type Obs[_] = TypeNme ==>> FieldNme => Error \/ SSelect

  object CoupleLogicTripletFun extends Logic[λ[x => TripletFun], Obs]
  import CoupleLogicTripletFun.differences

  "Optica" should "translate difference into a SELECT statement" in {
    differences(==>>("Person" -> "name")).map(_.toString) shouldBe
      \/-(
        "SELECT w.name, (w.age - m.age) FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name INNER JOIN Person AS m ON c.him = m.name WHERE (w.age > m.age)")
  }

  it should "be a correct query for a db" in {

    val persons = List(
      Couple(Person("Alex", 60), Person("Bert", 55)),
      Couple(Person("Cora", 33), Person("Drew", 31)),
      Couple(Person("Edna", 21), Person("Fred", 60))
    )

    Utils.transactor.use(transIO =>
      for {
        select <- differences(==>>("Person" -> "name")).fold(IO.raiseError, IO.pure)
        _ <- Utils.prepareCoupleEnviroment(persons).transact(Utils.xa)
        result <- Query0[(String, Int)](select.toString).to[List].transact(transIO)
      } yield result).unsafeRunSync shouldBe List("Alex" -> 5, "Cora" -> 2)

  }
}
