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
  lazy val tokenParts: Parser[Seq[String]] = P(tokenChars.!.rep(sep = ".", min = 1))
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

object ArchieParser extends CommonParsers {
  def PL[V](p: P[V]) = P(space.? ~ p ~ space.? ~ "\n")

  val SpecialTokens = Seq("skip", "endskip", "end", "ignore")

  implicit def liftParserToJsString(p: P[String]): P[JsString] = p.map(JsString.apply)
  //  implicit def liftParserToJsObject(p: P[Unit]): P[JsObject] = p.map(_ => JsObject.empty)

  implicit val jsObjectRepeater = new Repeater[JsObject, JsObject] {
    override type Acc = AtomicReference[JsObject]
    override def result(acc: Acc) = acc.get()
    override def initial = new AtomicReference(JsObject.empty)
    override def accumulate(t: JsObject, acc: Acc) = acc.set(Js.mergeObjects(acc.get(), t))
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
        case (None, _) => array.copy(elems :+ t)
        case (Some(JsArray.empty), _) => JsArray(t)
        case (Some(elem: JsObject), t: JsObject) if isNewObj(elem, t) => array.copy(elems :+ t)
        case (Some(elem: JsObject), t: JsObject) => array.copy(elems.init :+ Js.mergeObjects(elem, t))
        case (_, arr: JsArray) if arr.elements.isEmpty => array
        case (Some(elem), _) => array.copy(elems :+ t)
      }
    }
  }

  lazy val scopeText = P(space.? ~ "{" ~ tokenParts ~ "}" ~ space.?)
  lazy val resetScopeText = P(space.? ~ "{" ~ space.? ~ "}")
  lazy val resetArrayText = P(space.? ~ "[" ~ space.? ~ "]" ~ space.?)

  def textExcluding(exl: P[Any]) = P(!exl ~ rawText).rep(0, "\n") ~ "\n".?
  lazy val rawText = CharsWhile(pred = !"\n".contains(_: Char), min = 0)
  lazy val textAsJArr = P(!(resetScopeText | scopeText | resetArrayText) ~ rawText).map(_ => JsArray.empty)
  lazy val text = P(!(resetScopeText | scopeText | resetArrayText) ~ rawText).map(_ => JsObject.empty)

  lazy val multilineStr = P(strChars.!.rep(sep = "\n\\", min = 0)).map(strSeq => strSeq.mkString("\n"))

  def kvLine(ctx: Ctx = Ctx.Initial) = P(space.? ~ tokenParts ~ space.? ~ ":" ~ space.? ~ multilineStr).map { case (tp, v) =>
    ctx.jsObj(tp, JsString(v))
  }

  def resetScope(ctx: Ctx) = P(resetScopeText ~ "\n").flatMap { _ => line(ctx.scopePopped) }

  def scope(ctx: Ctx): P[JsObject] = P(space.? ~ "{" ~ space.? ~ tokenParts ~ space.? ~ "}\n").flatMap { tp =>
    line(ctx.scopePushed(Path.fromTP(tp)))
  }

  def arrayLines(ctx: Ctx): P[JsArray] = P((arrayStringLine ~ "\n" ~ stringArray) | kvArray
  ).log("arrayLines").map {
    case jArr: JsArray => jArr
    case (jv: JsValue, jArr: JsArray) => JsArray(jv +: jArr.elements)
    case _ => JsArray.empty
  }
  lazy val kvArray: P[JsArray] = P(kvLine(Ctx.InArray) | textAsJArr).rep(1, "\n")(jsArrayRepeater)
  lazy val stringArray: P[JsArray] = P(arrayStringLine | textAsJArr).rep(1, "\n")(jsArrayRepeater)
  lazy val arrayStringLine: P[JsString] = P(space.? ~ "*" ~ space.? ~ strChars.! ~ space.?)

  def array(ctx: Ctx): P[JsObject] =
    (PL("[" ~ space.? ~ tokenParts ~ space.? ~ "]") ~ textExcluding(arrayStringLine | kvLine(ctx) | resetArrayText)
      ~ arrayLines(Ctx.InArray).? ~ ("\n".? ~ resetArray).?).map {
      case (tp, Some(arr)) => ctx.jsObj(tp, arr)
      case (tp, None) => ctx.jsObj(tp, JsArray.empty)
    }

  lazy val resetArray: P[Unit] = PL("[" ~ space.? ~ "]")

  def line(ctx: Ctx): P[JsObject] =
    P(scope(ctx) | resetScope(ctx) | array(ctx) | kvLine(ctx) | text).rep(0, "\n")

  lazy val archieml = line(Ctx.Initial) ~ End

}

case class Ctx(scopeStack: List[Path] = Nil) {
  def scopePopped = scopeStack match {
    case Nil => this
    case _ => copy(scopeStack = scopeStack.tail)
  }

  def scopePushed(p: Path) = copy(scopeStack = p :: scopeStack)

  def contextualPath(tp: Seq[String]): Path = contextualPath(Path.fromTP(tp))

  def contextualPath(path: Path): Path = scopeStack.headOption match {
    case None => path
    case Some(thatPath) => path.merged(thatPath)
  }

  def jsObj(path: Path, v: JsValue) = contextualPath(path).jsObj(v)
  def jsObj(tp: Seq[String], v: JsValue) = contextualPath(tp).jsObj(v)

}

object Ctx {
  val Initial = Ctx()
  val InArray = Ctx()
}

case class Path(elements: List[String]) {
  def merged(path: Path) = Path(path.elements ::: elements)

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

