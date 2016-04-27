package org.json4s.typify

import org.json4s.{JValue, JObject, JString, JInt, JNothing, JNull}
import typify.{CanParse, Parsed, ParseError}
import scala.reflect.ClassTag
import scalaz.syntax.validation._
import scalaz.ValidationNel

object parsedinstances {
  lazy implicit val cpjv = new CanParse[JValue, JValue] {
    def parse(k: String, jv: JValue)(implicit ct: ClassTag[JValue]) = (jv \ k) match {
      case s: JValue => s.successNel[ParseError]
      case _ => ParseError(k, "Could not be parsed as JValue").failureNel[JValue]
    }
  }

  lazy implicit val cpjs = new CanParse[String, JValue] {
    def parse(k: String, jv: JValue)(implicit ct: ClassTag[String]) = (jv \ k) match {
      case JString(s) => s.successNel[ParseError]
      case _ => ParseError(k, "Could not be parsed as string").failureNel[String]
    }
  }
  lazy implicit val cpji = new CanParse[Int, JValue] {
    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Int]) = (jv \k) match {
      case JInt(i) => i.toInt.successNel[ParseError]
      case _ => ParseError(k, "Could not be parsed as int").failureNel[Int]
    }
  }
  lazy implicit val cpjos = new CanParse[Option[String], JValue] {
    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[String]]) = (jv \ k) match {
      case JString(s) => Some(s).successNel[ParseError]
      case JNothing | JNull => None.successNel[ParseError]
      case _ => ParseError(k, "Could not be parsed as option[string]").failureNel[Option[String]]
    }
  }
  lazy implicit val cpjoi = new CanParse[Option[Int], JValue] {
    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[Int]]) = (jv \ k) match {
      case JInt(i) => Some(i.toInt).successNel[ParseError]
      case JNothing | JNull => None.successNel[ParseError]
      case _ => ParseError(k, "Could not be parsed as option[int]").failureNel[Option[Int]]
    }
  }

}
