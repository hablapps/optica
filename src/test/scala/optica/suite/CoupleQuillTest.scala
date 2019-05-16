package dev.habla
package optica
package suite

import org.scalatest._
import io.getquill._

import example._
import model._

import symantics.interpreter.{ctx, QObs, QGetAll}
import ctx._

class CoupleQuillTest extends FlatSpec with Matchers {

  case class CoupleRel(her: String, him: String)
  case class PersonRel(name: String, age: Int)

  val q: Quoted[Couples] = quote {
    for {
      c <- query[CoupleRel]
      w <- query[PersonRel]
      m <- query[PersonRel]
      if c.her == w.name && c.him == m.name
    } yield Couple(Person(w.name, w.age), Person(m.name, m.age))
  }

  object CoupleLogicQuill extends CoupleLogic[λ[x => x], QObs]
  import CoupleLogicQuill.differences

  differences match {
    case QGetAll(f) => f(q)
  }
}

