package me.maciejb.sarchieml

import fastparse.core.Result
import spray.json.JsObject

object ArchiemlParser {

  def parse(input: String): JsObject = {
    NoLoggingArchiemlParsers.archieml.parse(input) match {
      case Result.Success(v, _) => v
      case f: Result.Failure => sys.error("Failure while parsing input.\n" +
        "This should never normally happen. Please fill in an issue on github.com/maciej/sarchieml\n" +
        "attaching the input document, if possible.")
    }
  }
}
