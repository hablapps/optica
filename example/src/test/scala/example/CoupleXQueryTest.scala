package example
package suite

import org.scalatest._
import xquery._
import model._

class CoupleXQueryTest extends FlatSpec with Matchers {

  object CoupleLogicXQuery extends CoupleLogic[λ[x => XQuery], λ[x => XQuery]]
  import CoupleLogicXQuery.differences

  "Optica" should "translate differences into an XQuery expression" in {
    differences.toString shouldBe "/xml/couple[her/age > him/age]/<tuple><fst>{her/name}</fst><snd>{her/age - him/age}</snd></tuple>"
  }
}

