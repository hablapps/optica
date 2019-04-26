package dev.habla
package optica
package suite

import org.scalatest._
import xquery._
import model._

class CoupleXQueryTest extends FlatSpec with Matchers {

  object CoupleLogicXQuery extends CoupleLogic[λ[x => XQuery]]
  import CoupleLogicXQuery.differences

  "Optica" should "translate differences into a fold" in {
    differences.toString shouldBe "couple[her/age > him/age]/<tuple><fst>{her/name}</fst><snd>{her/age - him/age}</snd></tuple>"
  }
}

