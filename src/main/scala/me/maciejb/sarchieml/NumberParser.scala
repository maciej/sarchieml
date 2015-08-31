package me.maciejb.sarchieml

import fastparse.all._
import spray.json.JsNumber

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
