package play.api.libs.json.typify

import play.api.libs.json.{JsArray, JsNull, JsValue, Reads}
import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import typify.{CanParse, Op, ParsedValidated}

trait CatchAllInstance {
  implicit class JsValueOps(jv: JsValue) {
    def asO[A: Reads](f: String => Option[A]): Option[A] = jv.asOpt[A].orElse(jv.asOpt[String].flatMap(f))
  }

  protected def gen[A: Reads](retry: String => Option[A])(implicit ct: ClassTag[A]): (CanParse[A, JsValue], CanParse[Option[A], JsValue], CanParse[List[A], JsValue], CanParse[Option[List[A]], JsValue]) =
    (new CanParse[A, JsValue] {
      def as(jv: JsValue): ParsedValidated[A] =
        jv.asO(retry).fold(Op.typeValueError(none[A]))(Op.typeValue(_))

      def parse(k: String, jv: JsValue): ParsedValidated[A] =
        (jv \ k).toOption.fold(Op.downFieldError[JsValue](k))(Op.downField(_, k)).flatMap(as(_))
    },
    new CanParse[Option[A], JsValue] {
      def as(jv: JsValue): ParsedValidated[Option[A]] =
        jv match {
          case JsNull => Op.typeValue(none[A])
          case v => v.asO(retry).fold(Op.typeValueError[Option[A]](None))(a => Op.typeValue(some(a)))
        }

      def parse(k: String, jv: JsValue): ParsedValidated[Option[A]] =
        Op.downField((jv \ k).getOrElse(JsNull), k).flatMap(as(_))
    },
    new CanParse[List[A], JsValue] {
      def as(jv: JsValue): ParsedValidated[List[A]] =
        for {
          jvs <- Some(jv).collect { case JsArray(vs) => Op.typeValue(vs.toList) }.getOrElse(Op.typeValueError(none[List[JsValue]]))
          res <- jvs.zipWithIndex.traverse(t => Op.arrayIndex(t._1, t._2).flatMap(
            v => v.asO(retry).fold(Op.typeValueError(none[A]))(Op.typeValue(_))))
        } yield res

      def parse(k: String, jv: JsValue): ParsedValidated[List[A]] =
        (jv \ k).toOption.fold(Op.downFieldError[JsValue](k))(Op.downField(_, k)).flatMap(as(_))
    },
    new CanParse[Option[List[A]], JsValue] {
      def as(jv: JsValue): ParsedValidated[Option[List[A]]] =
        jv match {
          case JsArray(vs) => vs.zipWithIndex.toList.traverse(t => Op.arrayIndex(t._1, t._2).flatMap(
            v => v.asO(retry).fold(Op.typeValueError(none[A]))(Op.typeValue(_)))).map(some(_))
          case _ => Op.typeValue(none[List[A]])
        }

      def parse(k: String, jv: JsValue): ParsedValidated[Option[List[A]]] =
        Op.downField((jv \ k).getOrElse(JsNull), k).flatMap(as(_))
    })

  implicit def cp[A: ClassTag: Reads]: CanParse[A, JsValue] = gen(_ => none[A])._1
  implicit def cpo[A: ClassTag: Reads]: CanParse[Option[A], JsValue] = gen(_ => none[A])._2
  implicit def cplo[A: ClassTag: Reads]: CanParse[List[A], JsValue] = gen(_ => none[A])._3
  implicit def cpolo[A: ClassTag: Reads]: CanParse[Option[List[A]], JsValue] = gen(_ => none[A])._4
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
