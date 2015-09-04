package me.maciejb.sarchieml

import java.util.concurrent.atomic.AtomicReference

import fastparse.Implicits.Repeater
import fastparse.all._
import spray.json._

import scala.language.implicitConversions

trait CommonParsers {
  lazy val space = CharsWhile(" \t".contains(_: Char))
  lazy val strChars = CharsWhile(!"\n".contains(_: Char))
  lazy val tokenChars =
    (CharPred(c => CharPredicates.isLetter(c) || CharPredicates.isDigit(c)) | CharIn("_-")).rep(1)

  lazy val tokenName: Parser[String] = tokenChars.rep(1).!
  lazy val tokenPath: Parser[Path] = P(tokenChars.!.rep(sep = ".", min = 1)) map Path.fromTP

  lazy val ws = space.?
}

object Js {
  def mergeObjects(j1: JsObject, j2: JsObject): JsObject = {
    val f1 = j1.fields
    val f2 = j2.fields
    f2.headOption match {
      case None => j1
      case Some((k, v2)) =>
        (f1.get(k), v2) match {
          case (Some(internal: JsObject), v2o: JsObject) =>
            JsObject(f1.tail + (k -> mergeObjects(internal, v2o)))
          case (Some(internal: JsObject), _) => JsObject(f1.tail + (k -> v2))
          //            case (Some(_), arr2: JsArray) if arr2.elements.isEmpty => mergeJsObjects(JsObject(f1), JsObject(f2.tail))
          case (_, _) => mergeObjects(JsObject(f1 + (k -> v2)), JsObject(f2.tail))
        }
    }
  }
}

case class FlattenJsObject(fields: Map[String, JsValue]) {
  def toJsObj = JsObject(fields)
}

object ArchieParser extends CommonParsers {

  def PL[V](p: P[V]) = P(space.? ~ p ~ space.? ~ "\n")

  val SpecialTokens = Seq("skip", "endskip", "end", "ignore")

  val jsObjectRepeater = new Repeater[Any, JsObject] {
    override type Acc = AtomicReference[JsObject]
    override def result(acc: Acc) = acc.get()
    override def initial = new AtomicReference(JsObject.empty)
    override def accumulate(n: Any, acc: Acc) = n match {
      case t: FlattenJsObject => acc.set(JsObject(acc.get().fields ++ t.fields))
      case t: JsObject => acc.set(Js.mergeObjects(acc.get(), t))
      case _ => sys.error("should not happen")
    }
  }

  val jsArrayRepeater = new Repeater[JsValue, JsArray] {
    override type Acc = AtomicReference[JsArray]
    override def result(acc: Acc) = acc.get()
    override def initial = new AtomicReference(JsArray.empty)

    override def accumulate(t: JsValue, acc: Acc) = {
      acc.set(merge(acc.get(), t))
    }

    def merge(array: JsArray, t: JsValue): JsArray = {
      val elems = array.elements
      def isNewObj(prev: JsObject, next: JsObject) = prev.fields.keys.exists(next.fields.contains)
      (elems.lastOption, t) match {
        case (None | Some(JsArray.empty), _) => JsArray(t)
        case (Some(elem: JsObject), t: JsObject) if isNewObj(elem, t) => array.copy(elems :+ t)
        case (Some(elem: JsObject), t: JsObject) => array.copy(elems.init :+ Js.mergeObjects(elem, t))
        case (_, arr: JsArray) if arr.elements.isEmpty => array
        case (Some(elem), _) => array.copy(elems :+ t)
      }
    }
  }

  lazy val scopeMarker: P[Unit] = P(ws ~ "{" ~ tokenPath ~ "}" ~ ws).map(_ => ())
  lazy val resetScopeMarker = P(ws ~ "{" ~ ws ~ "}" ~ ws)
  lazy val resetArrayMarker = P(ws ~ "[" ~ ws ~ "]" ~ ws)
  lazy val subArrayMarker = P("[." ~ tokenChars ~ "]" ~ ws ~ "\n")
  lazy val markers = P(scopeMarker | resetScopeMarker | resetArrayMarker | subArrayMarker)

  def textExcluding(exl: P[Any]) = P(!exl ~ rawText).rep(0, "\n") ~ "\n".?
  def textLineExcluding(exl: P[Any]) = P(!exl ~ rawText).log("tle")
  lazy val rawText = CharsWhile(pred = !"\n".contains(_: Char), min = 0)
  lazy val textAsJArr = P(!(resetScopeMarker | scopeMarker | resetArrayMarker) ~ rawText).map(_ => JsArray.empty)
  lazy val anyText = P(rawText).map(_ => JsObject.empty)

  lazy val kvLine: P[JsObject] =
    P(ws ~ tokenPath ~ ws ~ ":" ~ ws ~ multilineStr).log("kvLine").map { case (p, v) => p.jsObj(JsString(v)) }

  lazy val multilineStr = P(strChars.!.rep(sep = "\n\\", min = 0)).map(strSeq => strSeq.mkString("\n"))

  lazy val scope: P[JsObject] =
    P(ws ~ "{" ~ ws ~ tokenPath ~ ws ~ "}" ~ ws ~ "\n").log("scope").flatMap(scopeContent)

  def scopeContent(path: Path): P[JsObject] =
    P((kvLine | array | scope.map(o => FlattenJsObject(o.fields))
      | textLineExcluding(resetScopeMarker)).rep(0, "\n")
      ~ (("\n" ~ resetScopeMarker ~ "\n".?) | End)).log("scopeContent").map { seq =>
      seq.foldLeft(JsObject.empty) {
        case (acc, jsObj: JsObject) => Js.mergeObjects(acc, path.jsObj(jsObj))
        case (acc, scopeObj: FlattenJsObject) => Js.mergeObjects(acc, scopeObj.toJsObj)
        case (acc, _) => acc
      }
    }

  lazy val array: P[JsObject] =
    (PL("[" ~ space.? ~ tokenPath ~ space.? ~ "]") ~ textExcluding(arrayStringLine | kvLine
      | resetArrayMarker | subArrayMarker)
      ~ arrayLines.? ~ ("\n".? ~ resetArrayMarker).?).log("array").map {
      case (path, Some(arr)) => path.jsObj(arr)
      case (path, None) => path.jsObj(JsArray.empty)
    }

  lazy val arrayLines: P[JsArray] = P((subArray | (arrayStringLine ~ "\n" ~ stringArray.?) | kvArray) ~ "\n".?).
    log("arrayLines").map {
    case jObj: JsObject => JsArray(jObj)
    case jArr: JsArray => jArr
    case (jv: JsValue, Some(jArr: JsArray)) => JsArray(jv +: jArr.elements)
    case (jv: JsValue, None) => JsArray(jv)
    case _ => JsArray.empty
  }

  lazy val kvArray: P[JsArray] = P(subArray | kvLine | textAsJArr).rep(1, "\n")(jsArrayRepeater).
    log("kvArray")
  lazy val stringArray: P[JsArray] = P(subArray | arrayStringLine | textAsJArr).log("stringArray")
    .rep(1, "\n")(jsArrayRepeater)
  lazy val arrayStringLine: P[JsString] = P(ws ~ "*" ~ ws ~ strChars.! ~ ws).map(JsString.apply)

  lazy val subArray: P[JsObject] = P("[." ~ tokenChars.! ~ "]" ~ ws ~ "\n").log("subArray").flatMap(subArrayContent)

  def subArrayContent(scope: String): P[JsObject] = P(arrayLines.? ~ (resetArrayMarker | End)).log("subArrayContent") map {
    case Some(arr) => JsObject(scope -> arr)
    case None => JsObject(scope -> JsArray.empty)
  }

  lazy val freeformArray = P(ws ~ "[+" ~ ws ~ tokenPath ~ ws ~ "]" ~ ws ~ "\n").log("freeformArray")
    .flatMap(freeformArrayContent)
  def freeformArrayContent(p: Path) = P(("\n".? ~ (kvFfArrLine | textExcluding(resetArrayMarker))).rep(0, "\n") ~ "\n".? ~
    (resetArrayMarker | End)).map { lines =>
    val list = lines.foldLeft(List[JsObject]()) {
      case (acc, (k: String, v: String)) => JsObject("type" -> JsString(k), "value" -> JsString(v)) :: acc
      case (acc, _) => acc
    }
    p.jsObj(JsArray(list.reverse.toVector))
  }

  lazy val kvFfArrLine: P[(String, String)] = P(ws ~ tokenChars.! ~ ws ~ ":" ~ ws ~ rawText.!).log("kvFfArrLine")

  lazy val archieml = P(kvLine | scope | array | freeformArray | anyText).rep(0, "\n")(jsObjectRepeater) ~ End

}

case class Path(elements: List[String]) {
  def jsObj(v: JsValue): JsObject = {
    def buildUp(e: List[String], v: JsValue): JsObject = e match {
      case Nil => sys.error("")
      case head :: Nil => JsObject(head -> v)
      case head :: tail => buildUp(tail, JsObject(head -> v))
    }
    buildUp(elements.reverse, v)
  }

}

object Path {
  def fromTP(elements: Seq[String]): Path = Path(elements.toList)
}

