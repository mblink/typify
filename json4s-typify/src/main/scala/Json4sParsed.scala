package org.json4s.typify

import org.json4s.{JValue, JObject, JArray, JBool, JDecimal, JDouble, JString, JInt, JLong, JNothing, JNull}
import typify.{CanParse, Parsed, ParseError}
import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.syntax.nel._
import scalaz.syntax.id._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.ValidationNel

object parsedinstances {
  lazy implicit val cpjv = new CanParse[JValue, JValue] {
    def parse(k: String, jv: JValue)(implicit ct: ClassTag[JValue]) = (jv \ k) match {
      case JNull | JNothing => ParseError(k, "Could not be parsed as JValue").failureNel[JValue]
      case s: JValue => s.successNel[ParseError]
      case _ => ParseError(k, "Could not be parsed as JValue").failureNel[JValue]
    }
    def as(jv: JValue)(implicit ct: ClassTag[JValue]) = jv match {
      case JNull | JNothing =>
        ParseError("_root_", "Could not be represented as JValue").failureNel[JValue]
      case s: JValue => s.successNel[ParseError]
    }
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

  implicit def cpjla[A: ClassTag](implicit cpa: CanParse[A, JValue]) = new CanParse[List[A], JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[List[A]]) = jv match {
      case JArray(l) => l.traverseU(cpa.as(_))
                         .leftMap(_ =>
                          ParseError("_root_", s"Could not be interpreted as ${ct}").wrapNel)
      case _ => ParseError("_root_", s"Could not be interpreted as ${ct}").failureNel[List[A]]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[List[A]]) =
      as(jv \ k).leftMap(_ => ParseError(k, s"Could not be parsed as ${ct}").wrapNel)
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
      case JString(s) =>
        s.parseInt.leftMap(_ =>
          ParseError("_root_", "Could not be interpreted as Int").wrapNel)
      case _ => ParseError("_root_", "Could not be interpreted as Int").failureNel[Int]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Int]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Int").wrapNel)
  }

  lazy implicit val cpjl = new CanParse[Long, JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Long]) = jv match {
      case JLong(l) => l.toLong.successNel[ParseError]
      case JInt(i) => i.toLong.successNel[ParseError]
      case JString(s) =>
        s.parseLong.leftMap(_ =>
          ParseError("_root_", "Could not be interpreted as Long").wrapNel)
      case _ => ParseError("_root_", "Could not be interpreted as Long").failureNel[Long]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Long]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Long").wrapNel)
  }

  lazy implicit val cpjd = new CanParse[Double, JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Double]) = jv match {
      case JDouble(d) => d.toDouble.successNel[ParseError]
      case JDecimal(bd) => bd.toDouble.successNel[ParseError]
      case JLong(l) => l.toDouble.successNel[ParseError]
      case JInt(i) => i.toDouble.successNel[ParseError]
      case JString(s) =>
        s.parseDouble.leftMap(_ =>
          ParseError("_root_", "Could not be interpreted as Double").wrapNel)
      case _ => ParseError("_root_", "Could not be interpreted as Double").failureNel[Double]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Double]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Double").wrapNel)
  }

  lazy implicit val cpjb = new CanParse[Boolean, JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Boolean]) = jv match {
      case JBool(b) => b.successNel[ParseError]
      case JString(s) =>
        s.parseBoolean.leftMap(_ =>
          ParseError("_root_", "Could not be interpreted as Boolean").wrapNel)
      case _ => ParseError("_root_", "Could not be interpreted as Boolean").failureNel[Boolean]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Boolean]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Boolean").wrapNel)
  }

  implicit def cpjola[A: ClassTag](implicit cpa: CanParse[A, JValue]) =
    new CanParse[Option[List[A]], JValue] {
      def as(jv: JValue)(implicit ct: ClassTag[Option[List[A]]]) = jv match {
        case JArray(l) => l.traverseU(cpa.as(_))
                           .map(Some(_))
                           .leftMap(_ =>
                            ParseError("_root_", s"Could not be interpreted as ${ct}").wrapNel)
        case JNothing | JNull => None.successNel[ParseError]
        case _ => ParseError("_root_", s"Could not be interpreted as ${ct}").failureNel[Option[List[A]]]
      }

      def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[List[A]]]) =
        as(jv \ k).leftMap(_ => ParseError(k, s"Could not be parsed as ${ct}").wrapNel)
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
      case JString(s) =>
        s.parseInt.map(Some(_))
          .leftMap(_ =>
            ParseError("_root_", "Could not be interpreted as Option[Int]").wrapNel)
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
      case JString(s) =>
        s.parseLong.map(Some(_))
          .leftMap(_ =>
            ParseError("_root_", "Could not be interpreted as Option[Long]").wrapNel)
      case JNothing | JNull => None.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[Long]").failureNel[Option[Long]]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[Long]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[Long]").wrapNel)
  }

  lazy implicit val cpjod = new CanParse[Option[Double], JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Option[Double]]) = jv match {
      case JDouble(d) => Some(d.toDouble).successNel[ParseError]
      case JDecimal(bd) => Some(bd.toDouble).successNel[ParseError]
      case JString(s) =>
        s.parseDouble.map(Some(_))
          .leftMap(_ =>
            ParseError("_root_", "Could not be interpreted as Option[Double]").wrapNel)
      case JNothing | JNull => None.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[Double]").failureNel[Option[Double]]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[Double]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[Double]").wrapNel)
  }

  lazy implicit val cpjob = new CanParse[Option[Boolean], JValue] {
    def as(jv: JValue)(implicit ct: ClassTag[Option[Boolean]]) = jv match {
      case JBool(b) => Some(b).successNel[ParseError]
      case JString(s) =>
        s.parseBoolean.map(Some(_))
          .leftMap(_ =>
            ParseError("_root_", "Could not be interpreted as Option[Boolean]").wrapNel)
      case JNothing | JNull => None.successNel[ParseError]
      case _ => ParseError("_root_", "Could not be interpreted as Option[Boolean]").failureNel[Option[Boolean]]
    }

    def parse(k: String, jv: JValue)(implicit ct: ClassTag[Option[Boolean]]) =
      as(jv \ k).leftMap(_ => ParseError(k, "Could not be parsed as Option[Boolean]").wrapNel)
  }
}
