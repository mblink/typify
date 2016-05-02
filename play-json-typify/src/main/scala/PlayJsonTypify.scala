package play.api.libs.json.typify

import play.api.libs.json.{JsValue, JsObject, JsString, JsNumber, JsUndefined, JsNull}
import typify.{CanParse, Parsed, ParseError}
import scala.reflect.ClassTag
import scalaz.syntax.nel._
import scalaz.syntax.validation._
import scalaz.ValidationNel

object parsedinstances {
  lazy implicit val cpjv = new CanParse[JsValue, JsValue] {
    def parse(k: String, jv: JsValue)(implicit ct: ClassTag[JsValue]) = (jv \ k) match {
      case s: JsValue => s.successNel[ParseError]
      case _ => ParseError(k, "Could not be parsed as JsValue").failureNel[JsValue]
    }
    def as(jv: JsValue)(implicit ct: ClassTag[JsValue]) = jv.successNel[ParseError]
  }

  lazy implicit val cpojv = new CanParse[Option[JsValue], JsValue] {
    def as(jv: JsValue)(implicit ct: ClassTag[Option[JsValue]]) = jv match {
      case _: JsUndefined | JsNull => None.successNel[ParseError]
      case s: JsValue => Some(s).successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[JsValue]").failureNel[Option[JsValue]]
    }

    def parse(k: String, jv: JsValue)(implicit ct: ClassTag[Option[JsValue]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[JsValue]").wrapNel)
  }

  lazy implicit val cpjs = new CanParse[String, JsValue] {
    def as(jv: JsValue)(implicit ct: ClassTag[String]) = jv match {
      case JsString(s) => s.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as String").failureNel[String]
    }

    def parse(k: String, jv: JsValue)(implicit ct: ClassTag[String]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as String").wrapNel)
  }

  lazy implicit val cpji = new CanParse[Int, JsValue] {
    def as(jv: JsValue)(implicit ct: ClassTag[Int]) = jv match {
      case JsNumber(i) => i.toInt.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Int").failureNel[Int]
    }

    def parse(k: String, jv: JsValue)(implicit ct: ClassTag[Int]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Int").wrapNel)
  }

  lazy implicit val cpjos = new CanParse[Option[String], JsValue] {
    def as(jv: JsValue)(implicit ct: ClassTag[Option[String]]) = jv match {
      case JsString(s) => Some(s).successNel[ParseError]
      case _: JsUndefined | JsNull => None.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[String]").failureNel[Option[String]]
    }

    def parse(k: String, jv: JsValue)(implicit ct: ClassTag[Option[String]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[String]").wrapNel)
  }

  lazy implicit val cpjoi = new CanParse[Option[Int], JsValue] {
    def as(jv: JsValue)(implicit ct: ClassTag[Option[Int]]) = jv match {
      case JsNumber(i) => Some(i.toInt).successNel[ParseError]
      case _: JsUndefined | JsNull => None.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[Int]").failureNel[Option[Int]]
    }

    def parse(k: String, jv: JsValue)(implicit ct: ClassTag[Option[Int]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[Int]").wrapNel)
  }

}
