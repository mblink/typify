package scala.scalajs.js.typify

import cats.instances.option._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.Dynamic
import typify._

trait ParsedInstancesLP {
  implicit def canParseList[A](implicit cpa: CanParse[A, Dynamic], ct: ClassTag[A]): CanParse[List[A], Dynamic] =
    parseList(_: Cursor[Dynamic])(
      ParseError(_, s"Could not be interpreted as List[$ct]").invalidNel[List[A]],
      cpa(_))

  implicit def canParseOption[A](implicit cpa: CanParse[A, Dynamic]): CanParse[Option[A], Dynamic] =
    c => c.focus.flatMap(Option(_).filterNot(js.isUndefined)).traverse(_ => cpa(c))
}

object parsedinstances extends ParsedInstancesLP {
  implicit val cpd: CanParse[Dynamic, Dynamic] =
    c => c.focus.filterNot(js.isUndefined).toValidNel(ParseError(c, "Could not be interpreted as Dynamic"))

  implicit val cpod: CanParse[Option[Dynamic], Dynamic] =
    _.focus.flatMap(Option(_).filterNot(js.isUndefined)).validNel[ParseError[Dynamic]]

  private def nf[A](a: => A): Either[Throwable, A] = Either.catchOnly[Throwable](a)

  private def gen[A](
    cast: Dynamic => A,
    retry: String => Option[A]
  )(implicit ct: ClassTag[A]): CanParse[A, Dynamic] =
    c => c.focus.filterNot(js.isUndefined)
      .flatMap(x => nf(cast(x)).toOption.orElse(nf(x.asInstanceOf[String]).toOption.flatMap(retry)))
      .toValidNel(ParseError(c, s"Could not be interpreted as $ct"))

  implicit val genDynamic: Generic[Dynamic] = new Generic[Dynamic] {
    def fromFields(fields: ListMap[String, Dynamic]): Dynamic =
      Dynamic.literal.applyDynamic("apply")(fields.toSeq:_*)

    def toFields(value: Dynamic): Option[ListMap[String, Dynamic]] =
      nf(js.Object.keys(value.asInstanceOf[js.Object])
        .map(k => (k -> value.selectDynamic(k)))).toOption.map(ListMap.from(_))

    def fromValues(values: Vector[Dynamic]): Dynamic =
      Dynamic.newInstance(Dynamic.global.Array)(values.toSeq:_*)

    def toValues(value: Dynamic): Option[Vector[Dynamic]] =
      nf(value.asInstanceOf[js.Array[Dynamic]].toVector).toOption
  }

  implicit lazy val ps: CanParse[String, Dynamic] = gen(_.asInstanceOf[String], _ => Option.empty[String])
  implicit lazy val pi: CanParse[Int, Dynamic] = gen(_.asInstanceOf[Int], _.parseInt)
  implicit lazy val pl: CanParse[Long, Dynamic] = gen(_.asInstanceOf[Long], _.parseLong)
  implicit lazy val pd: CanParse[Double, Dynamic] = gen(_.asInstanceOf[Double], _.parseDouble)
  implicit lazy val pb: CanParse[Boolean, Dynamic] = gen(_.asInstanceOf[Boolean], _.parseBoolean)
}
