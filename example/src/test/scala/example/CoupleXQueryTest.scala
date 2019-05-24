package example
package test

import _root_.org.basex.core._
import _root_.org.basex.query._
import _root_.org.scalatest._
import example.couple._
import optica.xquery._

import scala.collection.JavaConverters._

class CoupleXQueryTest extends FlatSpec with Matchers {

  object CoupleLogicXQuery extends Logic[λ[x => XQuery], λ[x => XQuery]]
  import CoupleLogicXQuery.differences

  "Optica" should "translate differences into an XQuery expression" in {
    differences.toString shouldBe 
      "/xml/couple[her/age > him/age]/<tuple><fst>{her/name}</fst><snd>{her/age - him/age}</snd></tuple>"
  }

  it should "work with a xml example" in {

    def process(query: String)(filePath: String): List[String] = {
      val xml = s"""doc('$filePath')""".stripMargin
      val str = s"""for $$x in $xml$query return data($$x)"""
      val context = new Context()
      new QueryProcessor(str, context).value().asScala.map(_.toString).toList
    }

    val query = differences.toString

    process(query)("example/src/test/resources/couple.xml") shouldBe 
      List(""""Alex5"""",""""Cora2"""")
  }
}

