package example
package test

import _root_.org.scalatest._
import optica._
import xquery._
import org._
import _root_.org.basex.core._
import _root_.org.basex.query._
import scala.collection.JavaConverters._

class OrgXQueryTest extends FlatSpec with Matchers {

  object OrgLogicXQuery extends Logic[λ[x => XQuery], λ[x => XQuery]]
  import OrgLogicXQuery.expertise

  "Optica" should "translate differences into an XQuery expression" in {
    expertise("abstract").toString shouldBe """/xml/department[not(exists(employee[not(exists(task/tsk[. = "abstract"]))]))]/dpt"""
  }

  it should "work with a xml example" in {

    def process(query: String)(filePath: String):List[String] = { // Create a query processor
      val xml = s"""doc('$filePath')""".stripMargin
      val str = s"""for $$x in $xml$query return data($$x)"""
      val context = new Context()
      new QueryProcessor(str, context).value().asScala.map(_.toString).toList
    }

    val query = expertise("abstract").toString

    process(query)("example/src/test/resources/org" +
      ".xml") shouldBe List(""""Quality"""", """"Research"""")

  }
}

