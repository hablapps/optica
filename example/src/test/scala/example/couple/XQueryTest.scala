package example
package couple

import _root_.org.scalatest._
import optica._
import xquery._

class XQueryTest extends FlatSpec with Matchers {

  object CoupleLogicXQuery extends Logic[λ[x => XQuery], λ[x => XQuery]]
  import CoupleLogicXQuery.differences

  "Optica" should "translate differences into an XQuery expression" in {
    differences.toString shouldBe "/xml/couple[her/age > him/age]/<tuple><fst>{her/name}</fst><snd>{her/age - him/age}</snd></tuple>"
  }
}

