package play.api.libs.json.typify

import play.api.libs.json.{JsValue, JsObject, JsString, JsDefined}
import play.api.libs.json.{JsNumber, JsUndefined, JsNull, Reads}
import typify.{CanParse, Parsed, ParseError}
import scala.reflect.ClassTag
import scalaz.syntax.nel._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.ValidationNel

trait CatchAllInstance {

  implicit def cp[T](implicit rd: Reads[T]) = new CanParse[T, JsValue] {
    def parse(k: String, jv: JsValue)(implicit ct: ClassTag[T]) =
      (jv \ k).asOpt[T]
        .toSuccessNel(ParseError(k, s"Could not be parsed as ${ct.toString}"))

    def as(jv: JsValue)(implicit ct: ClassTag[T]) =
      jv.asOpt[T]
        .toSuccessNel(ParseError("_root_", s"Could not be interpreted as ${ct.toString}"))
  }
}

object parsedinstances extends CatchAllInstance {

  implicit def parseO[T](implicit rd: Reads[T]) = new CanParse[Option[T], JsValue] {
   def parse(k: String, jv: JsValue)(implicit ct: ClassTag[Option[T]]):
    ValidationNel[ParseError, Option[T]] = (jv \ k) match {
      case _: JsUndefined => None.successNel[ParseError]
      case JsDefined(r) => as(r)
    }

    def as(jv: JsValue)(implicit ct: ClassTag[Option[T]]):
    ValidationNel[ParseError, Option[T]] = jv match {
      case JsNull => None.successNel[ParseError]
      case s: JsValue => s.asOpt[T]
                          .map(Some(_))
                          .toSuccessNel(ParseError("_root_",
                            s"Could not be interpreted as Option[${ct.toString}]"))
    }
  }
}
