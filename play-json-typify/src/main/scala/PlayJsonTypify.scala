package play.api.libs.json.typify

import play.api.libs.json.{JsDefined, JsNull, JsReadable, JsUndefined, JsValue, Reads}
import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import typify.{CanParse, Op, Validated, ValidatedHelper}

trait CatchAllInstance extends ValidatedHelper {
  private def gen0[A: Reads](retry: JsReadable => Option[A])(implicit ct: ClassTag[A]): (CanParse[A, JsValue], CanParse[Option[A], JsValue]) =
    (new CanParse[A, JsValue] {
      def as(jv: JsValue): Validated[A] =
        Validated(ops => jv.asOpt[A].orElse(retry(jv)).fold(Op.typedValueError[A](ops, none[A]))(Op.typedValue(_)))

      def parse(k: String, jv: JsValue): Validated[A] =
        Validated(ops => (jv \ k) match {
          case JsDefined(r) => Op.downField(r, k)
          case _: JsUndefined => Op.downFieldError[JsValue](ops, k)
        }).flatMap(as(_))
    },
    new CanParse[Option[A], JsValue] {
      def as(jv: JsValue): Validated[Option[A]] =
        Validated(ops => jv match {
          case JsNull => Op.typedValue(none[A])
          case v: JsValue => v.asOpt[A]
                              .orElse(retry(v))
                              .fold(Op.typedValueError[Option[A]](ops, none[A]))(a => Op.typedValue(some(a)))
        })

      def parse(k: String, jv: JsValue): Validated[Option[A]] =
        for {
          v <- Validated(_ => Op.downField((jv \ k).getOrElse(JsNull), k))
          r <- as(v)
        } yield r
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
