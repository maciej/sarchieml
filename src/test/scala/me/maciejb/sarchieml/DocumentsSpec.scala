package me.maciejb.sarchieml

import fastparse.core.Result
import org.apache.commons.io.{Charsets, IOUtils}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import spray.json._

import scala.collection.JavaConverters._

class DocumentsSpec extends FlatSpec {

  for (document <- Documents.testData) {
    it should s"parse document ${document.filePrefix}" in {
      document.test()
    }
  }

}

case class TestDocument(filePrefix: String) extends DefaultJsonProtocol {

  private[this] def archiemlPath: String = s"${Documents.DocumentsDir}/$filePrefix.archieml"
  private[this] def jsonPath: String = s"${Documents.DocumentsDir}/$filePrefix.json"

  private[this] def archiemlParsed: JsValue = {
    val inputStr = IOUtils.toString(getClass.getClassLoader.getResource(archiemlPath), Charsets.UTF_8)
    ArchieParser.archieml.parse(inputStr, 0) match {
      case Result.Success(jObj, _) => jObj
      case failure: Result.Failure => fail(s"${failure.msg}\n${failure.lastParser}")
    }
  }

  private[this] def json: JsValue = {
    import spray.json._
    IOUtils.toString(getClass.getClassLoader.getResource(jsonPath), Charsets.UTF_8).parseJson
  }

  def test(): Unit = {
    archiemlParsed shouldEqual json
  }
}

object Documents {
  val DocumentsDir = "documents/"

  def testData: Seq[TestDocument] = {
    val DocumentsDir = "documents/"
    val dirStream = getClass.getClassLoader.getResourceAsStream(DocumentsDir)
    val files = IOUtils.readLines(dirStream, Charsets.UTF_8).asScala
    for (file <- files if file.endsWith("json") && !file.contains(".disabled.")) yield {
      TestDocument(file.substring(0, file.indexOf(".")))
    }
  }
}