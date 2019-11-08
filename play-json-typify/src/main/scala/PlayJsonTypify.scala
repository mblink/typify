package play.api.libs.json.typify

import play.api.libs.json.{JsDefined, JsNull, JsReadable, JsUndefined, JsValue, Reads}
import typify.{CanParse, Op, Parsed, ParseError}
import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.ValidationNel

trait CatchAllInstance {
  private def gen0[A: Reads](retry: JsReadable => Option[A])(implicit ct: ClassTag[A]): (CanParse[A, JsValue], CanParse[Option[A], JsValue]) =
    (new CanParse[A, JsValue] {
      def as(p: Parsed[JsValue]): ValidationNel[ParseError[JsValue], Parsed[A]] =
        p.value.asOpt[A].orElse(retry(p.value)).map(x => p.next(Op.TypeValue(x)))
          .toSuccessNel(ParseError(p, "_root_", s"Could not be interpreted as $ct"))

      def parse(k: String, p: Parsed[JsValue]): ValidationNel[ParseError[JsValue], Parsed[A]] =
        (p.value \ k).asOpt[A].orElse(retry(p.value \ k)).map(x => p.next(Op.TypeValue(x)))
          .toSuccessNel(ParseError(p, k, s"Could not be parsed as $ct"))
    },
    new CanParse[Option[A], JsValue] {
      def as(p: Parsed[JsValue]): ValidationNel[ParseError[JsValue], Parsed[Option[A]]] =
        p.value match {
          case JsNull => p.next(Op.TypeValue(none[A])).successNel[ParseError[JsValue]]
          case v: JsValue => v.asOpt[A]
                              .orElse(retry(v))
                              .map(a => p.next(Op.TypeValue(Option(a))))
                              .toSuccessNel(ParseError(p, "_root_", s"Could not be interpreted as Option[$ct]"))
        }

      def parse(k: String, p: Parsed[JsValue]): ValidationNel[ParseError[JsValue], Parsed[Option[A]]] =
        (p.value \ k) match {
          case JsDefined(r) => as(p.next(r, Op.DownField(k))).leftMap(_.map(_.copy(key = k)))
          case _: JsUndefined => p.next(Op.TypeValue(none[A])).successNel[ParseError[JsValue]]
        }
    })

  protected def gen[A: ClassTag: Reads](fromStr: String => Option[A]): (CanParse[A, JsValue], CanParse[Option[A], JsValue], CanParse[List[A], JsValue], CanParse[Option[List[A]], JsValue]) = {
    val (a, oa) = gen0(_.asOpt[String].flatMap(fromStr))
    val (la, loa) = gen0(_.asOpt[List[String]].flatMap(_.traverse(fromStr)))
    (a, oa, la, loa)
  }

  implicit def cp[A: ClassTag: Reads]: CanParse[A, JsValue] = gen0(_ => none[A])._1
  implicit def cpo[A: ClassTag: Reads]: CanParse[Option[A], JsValue] = gen0(_ => none[A])._2
  implicit def cplo[A: ClassTag: Reads]: CanParse[List[A], JsValue] = gen0(_ => none[List[A]])._1
  implicit def cpolo[A: ClassTag: Reads]: CanParse[Option[List[A]], JsValue] = gen0(_ => none[List[A]])._2
}

object parsedinstances extends CatchAllInstance {
  implicit lazy val (pi: CanParse[Int, JsValue], pio: CanParse[Option[Int], JsValue],
                     pli: CanParse[List[Int], JsValue], plio: CanParse[Option[List[Int]], JsValue]) =
    gen(_.parseInt.toOption)

  implicit lazy val (pl: CanParse[Long, JsValue], plo: CanParse[Option[Long], JsValue],
                     pll: CanParse[List[Long], JsValue], pllo: CanParse[Option[List[Long]], JsValue]) =
    gen(_.parseLong.toOption)

  implicit lazy val (pd: CanParse[Double, JsValue], pdo: CanParse[Option[Double], JsValue],
                     pld: CanParse[List[Double], JsValue], pldo: CanParse[Option[List[Double]], JsValue]) =
    gen(_.parseDouble.toOption)

  implicit lazy val (pb: CanParse[Boolean, JsValue], pbo: CanParse[Option[Boolean], JsValue],
                     plb: CanParse[List[Boolean], JsValue], plbo: CanParse[Option[List[Boolean]], JsValue]) =
    gen(_.parseBoolean.toOption)
}
