package me.maciejb.sarchieml

import java.util.concurrent.atomic.AtomicReference

import fastparse.Implicits.Repeater
import fastparse.Logger
import fastparse.all._
import spray.json._

import scala.language.implicitConversions

private[sarchieml] trait CommonParsers {
  lazy val space = CharsWhile(" \t".contains(_: Char))
  lazy val strChars = CharsWhile(!"\n".contains(_: Char))
  lazy val tokenChars =
    (CharPred(c => CharPredicates.isLetter(c) || CharPredicates.isDigit(c)) | CharIn("_-")).rep(1)

  lazy val tokenName: Parser[String] = tokenChars.rep(1).!
  lazy val tokenPath: Parser[Path] = P(tokenChars.!.rep(sep = ".", min = 1)) map Path.fromTP

  lazy val ws = space.?
}

private[sarchieml] object Js {
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

private[sarchieml] case class FlattenJsObject(fields: Map[String, JsValue]) {
  def toJsObj = JsObject(fields)
}

private[sarchieml] object ArchiemlParsers extends ArchiemlParsers

private[sarchieml] object NoLoggingArchiemlParsers extends ArchiemlParsers {
  override implicit val logger: Logger = NilLogger
}

private[sarchieml] trait ArchiemlParsers extends CommonParsers {
  implicit val logger: Logger = Logger.stdout

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
  lazy val endOfBlockMarker = P(":end")
  lazy val markers = P(scopeMarker | resetScopeMarker | resetArrayMarker | subArrayMarker | endOfBlockMarker)

  def textExcluding(exl: P[Any]) = P(!exl ~ rawText).rep(0, "\n") ~ "\n".?
  def tle(exl: P[Any]) = P(!exl ~ rawText).log("tle")
  lazy val tlm = tle(markers)
  lazy val rawText = CharsWhile(pred = !"\n".contains(_: Char), min = 0)
  lazy val textAsJArr = P(!(resetScopeMarker | scopeMarker | resetArrayMarker) ~ rawText).map(_ => JsArray.empty)
  lazy val anyText = P(rawText).map(_ => JsObject.empty)

  lazy val kToken = P(ws ~ tokenPath ~ ws ~ ":" ~ ws)

  lazy val kvLine: P[JsObject] =
    P(kToken ~ (multiLineStr | singleLineStr))
      .log("kvLine").map { case (p, v) => p.jsObj(JsString(v)) }

  lazy val escapedLine: P[String] = P("\\" ~ strChars.!).log("esc")

  lazy val singleLineStr = P(strChars.!)
  lazy val multiLineStr = P(strChars.! ~ "\n" ~ (escapedLine | tle(markers | kToken).!).rep(0, "\n")
    ~ "\n".? ~ (!"\\" ~ ":end"))
    .log("ml").map { case (s1, strings) => (s1 +: strings).mkString("\n") }

  lazy val scope: P[JsObject] =
    P(ws ~ "{" ~ ws ~ tokenPath ~ ws ~ "}" ~ ws ~ "\n").log("scope").flatMap(scopeContent)

  def scopeContent(path: Path): P[JsObject] =
    P((kvLine | blockComment | array | scope.map(o => FlattenJsObject(o.fields))
      | tle(resetScopeMarker)).rep(0, "\n")
      ~ (("\n" ~ resetScopeMarker ~ "\n".?) | End)).log("scopeContent").map { seq =>
      seq.foldLeft(JsObject.empty) {
        case (acc, jsObj: JsObject) => Js.mergeObjects(acc, path.jsObj(jsObj))
        case (acc, scopeObj: FlattenJsObject) => Js.mergeObjects(acc, scopeObj.toJsObj)
        case (acc, _) => acc
      }
    }

  lazy val array: P[JsObject] = P(ws ~ "[" ~ ws ~ tokenPath ~ ws ~ "]" ~ ws ~ "\n").log("array").flatMap(arrayContent)
  def arrayContent(p: Path) = P((subArray | blockComment | (arrayStringLine ~ "\n" ~ stringArray.?) | kvArray).?
    ~ "\n".? ~ (resetArrayMarker | End)).map { r =>
    val arr = r match {
      case Some(jObj: JsObject) => JsArray(jObj)
      case Some(jArr: JsArray) => jArr
      case Some((jv: JsValue, Some(jArr: JsArray))) => JsArray(jv +: jArr.elements)
      case Some((jv: JsValue, None)) => JsArray(jv)
      case _ => JsArray.empty
    }
    p.jsObj(arr)
  }

  lazy val kvArray: P[JsArray] = P(subArray | blockComment | kvLine | textAsJArr).rep(1, "\n")(jsArrayRepeater).
    log("kvArray")
  lazy val stringArray: P[JsArray] = P(subArray | blockComment | arrayStringLine | textAsJArr).log("stringArray")
    .rep(1, "\n")(jsArrayRepeater)
  lazy val arrayStringLine: P[JsString] = P(ws ~ "*" ~ ws ~ strChars.! ~ ws).map(JsString.apply)

  lazy val subArray: P[JsObject] = P("[." ~ tokenPath ~ "]" ~ ws ~ "\n").log("subArray").flatMap(arrayContent)

  lazy val freeformArray = P(ws ~ "[+" ~ ws ~ tokenPath ~ ws ~ "]" ~ ws ~ "\n").log("freeformArray")
    .flatMap(freeformArrayContent)
  def freeformArrayContent(p: Path) = P((ffSubScope | kvFfArrLine | textFfArrLine | Pass).rep(0, "\n") ~ "\n".? ~
    (resetArrayMarker | End)).map { lines =>
    val list = lines.foldLeft(List[JsObject]()) {
      case (acc, obj: JsObject) => obj :: acc
      case (acc, (k: String, v: String)) => JsObject("type" -> JsString(k), "value" -> JsString(v)) :: acc
      case (acc, t: String) if !t.isEmpty => JsObject("type" -> JsString("text"), "value" -> JsString(t)) :: acc
      case (acc, _) => acc
    }
    p.jsObj(JsArray(list.reverse.toVector))
  }

  lazy val ffSubScope: P[JsObject] = P(ws ~ "{." ~ tokenChars.! ~ ws ~ "}" ~ ws ~ "\n").flatMap(ffSubScopeContent)
  def ffSubScopeContent(sc: String) = P((kvFfArrLine | Pass).rep(0, "\n") ~ resetScopeMarker).map { lines =>
    val elems = lines.flatMap {
      case (k: String, v: String) => (k -> JsString(v)) :: Nil
      case _ => Nil
    }
    JsObject("type" -> JsString(sc), "value" -> JsObject(elems: _*))
  }

  lazy val blockComment = P(skipBlock | ignore).log("blockComment").map(_ => JsObject.empty)
  lazy val skipBlock = P(":skip" ~ ws ~ "\n" ~ (!":endskip" ~ strChars).rep(0, "\n") ~ "\n".? ~ (":endskip" | End))
  lazy val ignore = P(":ignore" ~ AnyChar.rep ~ End)

  lazy val kvFfArrLine: P[(String, String)] = P(ws ~ tokenChars.! ~ ws ~ ":" ~ ws ~ rawText.!).log("kvFfArrLine")
  lazy val textFfArrLine: P[String] = P(ws ~ tle(resetArrayMarker).! ~ ws)

  lazy val archieml = P(kvLine | scope | blockComment
    | array | freeformArray | anyText).rep(0, "\n")(jsObjectRepeater) ~ End
}


private[sarchieml] case class Path(elements: List[String]) {
  def jsObj(v: JsValue): JsObject = {
    def buildUp(e: List[String], v: JsValue): JsObject = e match {
      case Nil => sys.error("This should never happen")
      case head :: Nil => JsObject(head -> v)
      case head :: tail => buildUp(tail, JsObject(head -> v))
    }
    buildUp(elements.reverse, v)
  }

}

private[sarchieml] object Path {
  def fromTP(elements: Seq[String]): Path = Path(elements.toList)
}

