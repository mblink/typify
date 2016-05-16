package play.api.libs.json.typify

import play.api.libs.json.{JsValue, JsObject, JsString, JsDefined}
import play.api.libs.json.{JsNumber, JsUndefined, JsNull, Reads}
import typify.{CanParse, Parsed, ParseError}
import scala.reflect.ClassTag
import scalaz.syntax.nel._
import scalaz.syntax.std.option._
import scalaz.syntax.std.string._
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

  implicit def parseJSV = new CanParse[JsValue, JsValue] {
   def parse(k: String, jv: JsValue)(implicit ct: ClassTag[JsValue]):
    ValidationNel[ParseError, JsValue] = (jv \ k) match {
      case _: JsUndefined => JsNull.successNel[ParseError]
      case JsDefined(r) => as(r)
    }

    def as(jv: JsValue)(implicit ct: ClassTag[JsValue]):
    ValidationNel[ParseError, JsValue] = jv match {
      case JsNull => JsNull.successNel[ParseError]
      case s: JsValue => s.successNel[ParseError]
    }
  }

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

  implicit lazy val pi = new CanParse[Int, JsValue] {
    def parse(k: String, jv: JsValue)(implicit ct: ClassTag[Int]) =
      (jv \ k).asOpt[Int]
        .orElse((jv \ k).asOpt[String].flatMap(_.parseInt.toOption))
        .toSuccessNel(ParseError(k, s"Could not be parsed as Int"))

    def as(jv: JsValue)(implicit ct: ClassTag[Int]) =
      jv.asOpt[Int]
        .orElse(jv.asOpt[String].flatMap(_.parseInt.toOption))
        .toSuccessNel(ParseError("_root_", s"Could not be interpreted as Int"))
  }

  implicit lazy val pio = new CanParse[Option[Int], JsValue] {
   def parse(k: String, jv: JsValue)(implicit ct: ClassTag[Option[Int]]):
    ValidationNel[ParseError, Option[Int]] = (jv \ k) match {
      case _: JsUndefined => None.successNel[ParseError]
      case JsDefined(r) => as(r)
    }

    def as(jv: JsValue)(implicit ct: ClassTag[Option[Int]]):
    ValidationNel[ParseError, Option[Int]] = jv match {
      case JsNull => None.successNel[ParseError]
      case s: JsValue => s.asOpt[Int]
                          .orElse(s.asOpt[String].flatMap(_.parseInt.toOption))
                          .map(Some(_))
                          .toSuccessNel(ParseError("_root_",
                            s"Could not be interpreted as Option[Int]"))
    }
  }

  implicit lazy val pl = new CanParse[Long, JsValue] {
    def parse(k: String, jv: JsValue)(implicit ct: ClassTag[Long]) =
      (jv \ k).asOpt[Long]
        .orElse((jv \ k).asOpt[String].flatMap(_.parseLong.toOption))
        .toSuccessNel(ParseError(k, s"Could not be parsed as Long"))

    def as(jv: JsValue)(implicit ct: ClassTag[Long]) =
      jv.asOpt[Long]
        .orElse(jv.asOpt[String].flatMap(_.parseLong.toOption))
        .toSuccessNel(ParseError("_root_", s"Could not be interpreted as Long"))
  }

  implicit lazy val plo = new CanParse[Option[Long], JsValue] {
   def parse(k: String, jv: JsValue)(implicit ct: ClassTag[Option[Long]]):
    ValidationNel[ParseError, Option[Long]] = (jv \ k) match {
      case _: JsUndefined => None.successNel[ParseError]
      case JsDefined(r) => as(r)
    }

    def as(jv: JsValue)(implicit ct: ClassTag[Option[Long]]):
    ValidationNel[ParseError, Option[Long]] = jv match {
      case JsNull => None.successNel[ParseError]
      case s: JsValue => s.asOpt[Long]
                          .orElse(s.asOpt[String].flatMap(_.parseLong.toOption))
                          .map(Some(_))
                          .toSuccessNel(ParseError("_root_",
                            s"Could not be interpreted as Option[Long]"))
    }
  }
}
