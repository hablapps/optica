package dev.habla
package optica
package suite

import org.scalatest._

import example._
import model._

import symantics.interpreter._
import ctx._

class CoupleQuillTest extends FlatSpec with Matchers {

  object CoupleLogicQuill extends CoupleLogic[QRep, QObs]
  import CoupleLogicQuill.differences

  "Optica" should "translate differences into a Quill query" in {
    println(CoupleLogicQuill.simplest match {
      case QGetAll(f) => ctx.translate(f(quote { query[Couple].map(List(_)) }))
    })
  }
}

