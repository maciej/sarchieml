package me.maciejb.sarchieml

import fastparse.core.Parser
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import spray.json.{JsObject, JsString}

class ArchieParserSpec extends FunSpec {
  // See https://github.com/lihaoyi/fastparse/issues/34

  import fastparse.core.Result

  describe("Key-value line parser") {
    def ValidKvLines = Seq(
      "foo: 1" -> JsObject("foo" -> JsString("1")),
      "bar: -20" -> JsObject("bar" -> JsString("-20")),
      "foo: abcd" -> JsObject("foo" -> JsString("abcd")),
      "foo: a\n\\abcd" -> JsObject("foo" -> JsString("a\nabcd"))
    )

    testParser(ArchieParser.kvLine, ValidKvLines, "should parse a valid key-value line")
  }

  def testParser[V](parser: Parser[V], testData: Seq[(String, V)], testDesc: String) = {
    for ((text, expectedResult) <- testData) {
      it(s"$testDesc: $text") {
        val Result.Success(r, _) = parser.parse(text)
        r shouldEqual expectedResult
      }
    }
  }

  def testParserAgainstInvalidLines[V](parser: Parser[V], testData: Seq[String], testDesc: String) = {
    for (text <- testData) {
      it(s"$testDesc: $text") {
        parser.parse(text) match {
          case s: Result.Success[V] => fail("Parsing should've failed.")
          case _ =>
        }
      }
    }
  }

}