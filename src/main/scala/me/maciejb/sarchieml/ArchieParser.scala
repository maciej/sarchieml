package me.maciejb.sarchieml

import java.util.concurrent.atomic.AtomicReference

import fastparse.Implicits.Repeater
import fastparse.all._
import spray.json.{JsNumber, JsObject, JsString, JsValue}

import scala.collection.mutable

/*
 * Not used, as apparently ArchieML does not support number types
 */
trait NumberParser {
  val digits = P(CharsWhile('0' to '9' contains (_: Char)))
  val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
  val fractional = P("." ~ digits)
  val integral = P("0" | CharIn('1' to '9') ~ digits.?)

  val number: Parser[JsNumber] = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
    x => JsNumber(x.toDouble)
  )
}

trait CommonParsers {
  lazy val space = CharsWhile(" \t".contains(_: Char))
  lazy val strChars = CharsWhile(!"\n".contains(_: Char))
  lazy val tokenChars =
    (CharPred(c => CharPredicates.isLetter(c) || CharPredicates.isDigit(c)) | CharIn("_-")).rep(1)

  lazy val tokenName: Parser[String] = tokenChars.rep(1).!.map(t => t)
  lazy val tokenParts: Parser[Seq[String]] = P(tokenChars.!.rep(sep = ".", min = 0))
  lazy val tokenPart: Parser[String] = P("." ~ tokenChars.!)
}

object ArchieParser extends CommonParsers {

  val SpecialTokens = Seq("skip", "endskip", "end", "ignore")

  implicit val jsObjectRepeater = new Repeater[JsObject, JsObject] {
    override type Acc = AtomicReference[JsObject]
    override def result(acc: Acc) = acc.get()
    override def initial = new AtomicReference(JsObject.empty)
    override def accumulate(t: JsObject, acc: Acc) = acc.set(mergeJsObjects(acc.get(), t))

    private def mergeJsObjects(j1: JsObject, j2: JsObject): JsObject = {
      val f1 = j1.fields
      val f2 = j2.fields
      f2.headOption match {
        case None => j1
        case Some((k, v2)) =>
          (f1.get(k), v2) match {
            case (Some(internal: JsObject), v2o: JsObject) =>
              JsObject(f1.tail + (k -> mergeJsObjects(internal, v2o)))
            case (Some(internal: JsObject), _) => JsObject(f1.tail + (k -> v2))
            case (_, _) => mergeJsObjects(JsObject(f1 + (k -> v2)), JsObject(f2.tail))
          }
      }
    }

    private def asMutableMap[K, V](map: Map[K, V]): mutable.LinkedHashMap[K, V] = map match {
      case m: mutable.LinkedHashMap[K, V] => m
      case _ => mutable.LinkedHashMap(map.toSeq: _*)
    }

  }

  @deprecated("Use parts and value approach", "2015-08-31")
  def objKeyedByParts(k: Seq[String], v: JsValue): JsObject = {
    def buildUp(ks: List[String]): JsObject = ks match {
      case head :: Nil => JsObject(head -> v)
      case head :: tail => JsObject(head -> buildUp(tail))
      case Nil => JsObject.empty
    }
    buildUp(k.toList)
  }

  def kvPairToJsObject(p: (String, JsValue)) = JsObject(p)

  lazy val keyToken: P[String] =
    P(tokenName ~ ":" | ":" ~ tokenName | "{" ~ tokenName ~ "}" | "[" ~ tokenName ~ "]")
      .filter(t => !SpecialTokens.contains(t))

  lazy val allTokens: P[String] =
    P(tokenName ~ ":" | ":" ~ tokenName | "{" ~ tokenName ~ "}" | "[" ~ tokenName ~ "]")

  lazy val token: P[Token] = P(allTokens.map(t => Key(t)) |
    "{}".!.map(_ => EmptyToken) | "[]".!.map(_ => EmptyToken) | "*".!.map(_ => EmptyToken))

  lazy val scopeLine = P(space.? ~ "{" ~ tokenParts ~ "}" ~ space.?)
  lazy val resetScopeLine = P(space.? ~ "{" ~ space.? ~ "}")
  lazy val textLine = P(!(resetScopeLine | scopeLine) ~
    CharsWhile(pred = !"\n".contains(_: Char), min = 0)).map(_ => JsObject.empty)

  lazy val multilineStr = P(strChars.!.rep(sep = "\n\\", min = 0)).map(strSeq => strSeq.mkString("\n"))

  lazy val kvLine = P(space.? ~ tokenChars.! ~ (kvLineKeyPart | kvLineValue)).map(kvPairToJsObject)
  lazy val kvLineKeyPart: P[JsObject] = P(tokenPart ~ (kvLineKeyPart | kvLineValue)).map(kvPairToJsObject)
  lazy val kvLineValue = P(space.? ~ ":" ~ space.? ~ multilineStr).map(JsString(_))

  lazy val resetScope = P(resetScopeLine ~ "\n" ~ lineR)

  lazy val scope: P[JsObject] = P(space.? ~ "{" ~ tokenChars.! ~ (scopeKeyPart | scopeValue)).map(kvPairToJsObject)
  lazy val scopeKeyPart: P[JsObject] = P(tokenPart ~ (scopeKeyPart | scopeValue)).map(kvPairToJsObject)
  lazy val scopeValue: P[JsObject] = P(space.? ~ "}" ~ "\n" ~ lineR)

  lazy val line: P[JsObject] = P(kvLine | textLine)
  lazy val lineR = line.rep(sep = "\n", min = 0)

  lazy val archieml = (scope | resetScope | line).rep(sep = "\n", min = 0) ~ End

}

sealed trait Token
case class Key(str: String) extends Token
case object EmptyToken extends Token
