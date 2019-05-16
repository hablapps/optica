package dev.habla
package optica
package suite

import org.scalatest._

import example._
import model._

import symantics.interpreter._
import ctx._

class CoupleQuillTest extends FlatSpec with Matchers {

  case class CoupleRel(her: String, him: String)
  case class PersonRel(name: String, age: Int)

  val q: Quoted[Query[Couple]] = quote {
    for {
      c <- query[CoupleRel]
      w <- query[PersonRel]
      m <- query[PersonRel]
      if c.her == w.name && c.him == m.name
    } yield Couple(Person(w.name, w.age), Person(m.name, m.age))
  }

  object CoupleLogicQuill extends CoupleLogic[QRep, QObs]
  import CoupleLogicQuill.differences

  "Optica" should "translate differences into a Quill query" in {
    println(CoupleLogicQuill.simplest match {
      case QGetAll(f) => ctx.translate(f(quote { q }))
    })
  }
}

