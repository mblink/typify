package org.json4s.typify

import org.json4s.{JValue, JObject, JString, JInt, JLong, JNothing, JNull}
import typify.{CanParse, Parsed, ParseError}
import scala.reflect.ClassTag
import scalaz.syntax.nel._
import scalaz.syntax.validation._
import scalaz.ValidationNel

object parsedinstances {
  lazy implicit val cpjv = new CanParse[JValue, JValue] {
    def parse(k: String, jv: JValue)(implicit ct: ClassTag[JValue]) = (jv \ k) match {
      case s: JValue => s.successNel[ParseError]
      case _ => ParseError(k, "Could not be parsed as JValue").failureNel[JValue]
    }
    def as(jv: JValue)(implicit ct: ClassTag[JValue]) = jv.successNel[ParseError]
  }

  lazy implicit val cpojv = new CanParse[Option[JValue], JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Option[JValue]]) = jv match {
      case JNothing | JNull => None.successNel[ParseError]
      case s: JValue => Some(s).successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[JValue]").failureNel[Option[JValue]]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[JValue]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[JValue]").wrapNel)
  }

  lazy implicit val cpjs = new CanParse[String, JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[String]) = jv match {
      case JString(s) => s.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as String").failureNel[String]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[String]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as String").wrapNel)
  }

  lazy implicit val cpji = new CanParse[Int, JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Int]) = jv match {
      case JInt(i) => i.toInt.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Int").failureNel[Int]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Int]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Int").wrapNel)
  }

  lazy implicit val cpjl = new CanParse[Long, JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Long]) = jv match {
      case JLong(l) => l.toLong.successNel[ParseError]
      case JInt(i) => i.toLong.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Long").failureNel[Long]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Long]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Long").wrapNel)
  }

  lazy implicit val cpjos = new CanParse[Option[String], JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Option[String]]) = jv match {
      case JString(s) => Some(s).successNel[ParseError]
      case JNothing | JNull => None.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[String]").failureNel[Option[String]]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[String]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[String]").wrapNel)
  }

  lazy implicit val cpjoi = new CanParse[Option[Int], JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Option[Int]]) = jv match {
      case JInt(i) => Some(i.toInt).successNel[ParseError]
      case JNothing | JNull => None.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[Int]").failureNel[Option[Int]]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[Int]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[Int]").wrapNel)
  }

  lazy implicit val cpjol = new CanParse[Option[Long], JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Option[Long]]) = jv match {
      case JLong(l) => Some(l.toLong).successNel[ParseError]
      case JInt(i) => Some(i.toLong).successNel[ParseError]
      case JNothing | JNull => None.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[Long]").failureNel[Option[Long]]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[Long]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[Long]").wrapNel)
  }
}
