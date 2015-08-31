package me.maciejb.sarchieml

import java.util.concurrent.atomic.AtomicReference

import fastparse.Implicits.Repeater
import fastparse.all._
import spray.json._

import scala.language.implicitConversions

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

  lazy val tokenName: Parser[String] = tokenChars.rep(1).!
  lazy val tokenParts: Parser[Seq[String]] = P(tokenChars.!.rep(sep = ".", min = 1))
}

object ArchieParser extends CommonParsers {
  def PL[V](p: P[V]) = P(space.? ~ p ~ space.? ~ "\n")

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
  }

  lazy val scopeText = P(space.? ~ "{" ~ tokenParts ~ "}" ~ space.?)
  lazy val resetScopeText = P(space.? ~ "{" ~ space.? ~ "}")
  lazy val text = P(!(resetScopeText | scopeText) ~
    CharsWhile(pred = !"\n".contains(_: Char), min = 0)).map(_ => JsObject.empty)

  lazy val multilineStr = P(strChars.!.rep(sep = "\n\\", min = 0)).map(strSeq => strSeq.mkString("\n"))

  def kvLine(context: Context) = P(space.? ~ tokenParts ~ space.? ~ ":" ~ space.? ~ multilineStr).map { case (tp, v) =>
    context.jsObj(tp, JsString(v))
  }

  def resetScope(context: Context) = P(resetScopeText ~ "\n").flatMap { _ => line(context.scopePopped) }

  def scope(context: Context): P[JsObject] = P(space.? ~ "{" ~ space.? ~ tokenParts ~ space.? ~ "}\n").flatMap { tp =>
    line(context.scopePushed(Path.fromTP(tp)))
  }

  def array(context: Context): P[JsObject] = PL("[" ~ space.? ~ tokenParts ~ space.? ~ "]").map { tp =>
    context.jsObj(tp, JsArray.empty)
  }

  def line(context: Context): P[JsObject] =
    P(scope(context) | resetScope(context) | kvLine(context) | text).rep(sep = "\n", min = 0)

  lazy val archieml = line(Context.Initial) ~ End

}

case class Context(scopeStack: List[Path] = Nil) {
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

object Context {
  val Initial = Context()
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

sealed trait Token
case class Key(str: String) extends Token
case object EmptyToken extends Token
