package org.json4s.typify

import cats.syntax.option._
import cats.syntax.validated._
import org.json4s.{jvalue2readerSyntax, JArray, JNothing, JNull, JObject, JString, JValue, Reader}
import org.json4s.DefaultReaders._
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import typify._

trait CatchAllInstance {
  protected def gen0[A](retry: String => Option[A])(parse: PartialFunction[JValue, Option[A]])(implicit ct: ClassTag[A]): (CanParse[A, JValue], CanParse[Option[A], JValue], CanParse[List[A], JValue], CanParse[Option[List[A]], JValue]) = {
    val cpa: CanParse[A, JValue] = c =>
      c.focus.flatMap(j => parse.lift(j).flatten.orElse(j.getAs[String].flatMap(retry)))
        .toValidNel(ParseError(c, s"Could not be interpreted as $ct"))

    val cpla: CanParse[List[A], JValue] = parseList(_: Cursor[JValue])(
      ParseError(_, s"Could not be interpreted as List[$ct]").invalidNel[List[A]],
      cpa(_))

    def opt[B](cp: CanParse[B, JValue]): CanParse[Option[B], JValue] =
      c => c.focus match {
        case Some(JNull) => None.validNel[ParseError[JValue]]
        case Some(_) => cp(c).map(Some(_))
        case None => None.validNel[ParseError[JValue]]
      }

    (cpa, opt(cpa), cpla, opt(cpla))
  }

  protected def gen[A: Reader](retry: String => Option[A])(implicit ct: ClassTag[A]): (CanParse[A, JValue], CanParse[Option[A], JValue], CanParse[List[A], JValue], CanParse[Option[List[A]], JValue]) =
    gen0[A](retry) { case j => j.getAs[A] }

  implicit def cp[A: ClassTag: Reader]: CanParse[A, JValue] = gen(_ => none[A])._1
  implicit def cpo[A: ClassTag: Reader]: CanParse[Option[A], JValue] = gen(_ => none[A])._2
  implicit def cplo[A: ClassTag: Reader]: CanParse[List[A], JValue] = gen(_ => none[A])._3
  implicit def cpolo[A: ClassTag: Reader]: CanParse[Option[List[A]], JValue] = gen(_ => none[A])._4
}

object parsedinstances extends CatchAllInstance {
  implicit val genJValue: Generic[JValue] = new Generic[JValue] {
    def fromFields(fields: ListMap[String, JValue]): JValue = JObject(fields.toList)
    def toFields(value: JValue): Option[ListMap[String, JValue]] = Option(value).collect { case JObject(fs) => ListMap(fs:_*) }
    def fromValues(values: Vector[JValue]): JValue = JArray(values.toList)
    def toValues(value: JValue): Option[Vector[JValue]] = Option(value).collect { case JArray(vs) => vs.toVector }
  }


  implicit val cpJValue: CanParse[JValue, JValue] =
    c => c.focus.filterNot(x => x == JNull || x == JNothing).toValidNel(ParseError(c, "Could not be interpreted as JValue"))

  implicit val cpOJValue: CanParse[Option[JValue], JValue] =
    c => c.focus match {
      case Some(JNull | JNothing) => None.validNel[ParseError[JValue]]
      case Some(jv) => Some(jv).validNel
      case None => None.validNel[ParseError[JValue]]
    }

  implicit lazy val (ps: CanParse[String, JValue], pso: CanParse[Option[String], JValue],
                     pls: CanParse[List[String], JValue], plso: CanParse[Option[List[String]], JValue]) =
    gen0(_ => none[String]) { case JString(s) => Some(s) }

  implicit lazy val (pi: CanParse[Int, JValue], pio: CanParse[Option[Int], JValue],
                     pli: CanParse[List[Int], JValue], plio: CanParse[Option[List[Int]], JValue]) =
    gen(_.parseInt)

  implicit lazy val (pl: CanParse[Long, JValue], plo: CanParse[Option[Long], JValue],
                     pll: CanParse[List[Long], JValue], pllo: CanParse[Option[List[Long]], JValue]) =
    gen(_.parseLong)

  implicit lazy val (pd: CanParse[Double, JValue], pdo: CanParse[Option[Double], JValue],
                     pld: CanParse[List[Double], JValue], pldo: CanParse[Option[List[Double]], JValue]) =
    gen(_.parseDouble)

  implicit lazy val (pb: CanParse[Boolean, JValue], pbo: CanParse[Option[Boolean], JValue],
                     plb: CanParse[List[Boolean], JValue], plbo: CanParse[Option[List[Boolean]], JValue]) =
    gen(_.parseBoolean)
}
