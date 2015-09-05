package me.maciejb.sarchieml

import fastparse.all._
import fastparse.core.Result
import org.scalatest.FlatSpec

class EndOfBlockParserSpec extends FlatSpec {
  lazy val p: P[Unit] = P(!"\\" ~ ":end")

  it should "successfully parse a end of block marker" in {
    p.parse(":end") match {
      case f: Result.Failure => fail(f.toString())
      case _ => ()
    }
  }

  it should "fail to parse an escaped block marker" in {
    p.parse("\\:end") match {
      case s: Result.Success[Unit] => fail(s.toString)
      case _ => ()
    }
  }

}
