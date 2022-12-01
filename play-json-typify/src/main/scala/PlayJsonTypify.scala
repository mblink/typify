package play.api.libs.json.typify

import cats.syntax.option._
import cats.syntax.validated._
import play.api.libs.json.{JsArray, JsNull, JsObject, JsValue, Reads}
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import typify._

trait CatchAllInstance {
  protected def gen[A: Reads](retry: String => Option[A])(implicit ct: ClassTag[A]): (CanParse[A, JsValue], CanParse[Option[A], JsValue], CanParse[List[A], JsValue], CanParse[Option[List[A]], JsValue]) = {
    val cpa: CanParse[A, JsValue] = c =>
      c.focus.flatMap(jv => jv.asOpt[A].orElse(jv.asOpt[String].flatMap(retry)))
        .toValidNel(ParseError(c, s"Could not be interpreted as $ct"))

    val cpla: CanParse[List[A], JsValue] = parseList(_: Cursor[JsValue])(
      ParseError(_, s"Could not be interpreted as List[$ct]").invalidNel[List[A]],
      cpa(_))

    def opt[B](cp: CanParse[B, JsValue]): CanParse[Option[B], JsValue] =
      c => c.focus match {
        case Some(JsNull) => None.validNel[ParseError[JsValue]]
        case Some(_) => cp(c).map(Some(_))
        case None => None.validNel[ParseError[JsValue]]
      }

    (cpa, opt(cpa), cpla, opt(cpla))
  }

  implicit def cp[A: ClassTag: Reads]: CanParse[A, JsValue] = gen(_ => none[A])._1
  implicit def cpo[A: ClassTag: Reads]: CanParse[Option[A], JsValue] = gen(_ => none[A])._2
  implicit def cplo[A: ClassTag: Reads]: CanParse[List[A], JsValue] = gen(_ => none[A])._3
  implicit def cpolo[A: ClassTag: Reads]: CanParse[Option[List[A]], JsValue] = gen(_ => none[A])._4
}

object parsedinstances extends CatchAllInstance {
  implicit val genJsValue: Generic[JsValue] = new Generic[JsValue] {
    def fromFields(fields: ListMap[String, JsValue]): JsValue = JsObject(fields)
    def toFields(value: JsValue): Option[ListMap[String, JsValue]] = Some(value).collect {
      case o@JsObject(_) => ListMap.from(o.fields)
    }
    def fromValues(values: Vector[JsValue]): JsValue = JsArray(values)
    def toValues(value: JsValue): Option[Vector[JsValue]] = Some(value).collect {
      case JsArray(v) => v.toVector
    }
  }

  implicit val (pi: CanParse[Int, JsValue], pio: CanParse[Option[Int], JsValue],
                pli: CanParse[List[Int], JsValue], plio: CanParse[Option[List[Int]], JsValue]) =
    gen(_.parseInt)

  implicit val (pl: CanParse[Long, JsValue], plo: CanParse[Option[Long], JsValue],
                pll: CanParse[List[Long], JsValue], pllo: CanParse[Option[List[Long]], JsValue]) =
    gen(_.parseLong)

  implicit val (pd: CanParse[Double, JsValue], pdo: CanParse[Option[Double], JsValue],
                pld: CanParse[List[Double], JsValue], pldo: CanParse[Option[List[Double]], JsValue]) =
    gen(_.parseDouble)

  implicit val (pb: CanParse[Boolean, JsValue], pbo: CanParse[Option[Boolean], JsValue],
                plb: CanParse[List[Boolean], JsValue], plbo: CanParse[Option[List[Boolean]], JsValue]) =
    gen(_.parseBoolean)
}
