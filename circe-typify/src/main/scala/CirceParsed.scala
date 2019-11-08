package circe.api.libs.json.typify

import io.circe.{ACursor, Decoder, Json}
import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.ValidationNel
import typify.{CanParse, Op, Parsed, ParseError}

trait CatchAllInstance {
  private def iErr[A](implicit ct: ClassTag[A]): String = s"Could not be interpreted as $ct"
  private def pErr[A](implicit ct: ClassTag[A]): String = s"Could not be parsed as $ct"

  private def gen0[A: ClassTag: Decoder](retry: ACursor => Option[A]): (CanParse[A, Json], CanParse[Option[A], Json]) =
    (new CanParse[A, Json] {
      def as(p: Parsed[Json]): ValidationNel[ParseError[Json], Parsed[A]] =
        p.value.as[A].fold(_ => retry(p.value.hcursor), some(_))
          .map(x => p.next(Op.TypeValue(x)))
          .toSuccessNel(ParseError(p, "_root_", iErr[A]))

      def parse(k: String, p: Parsed[Json]): ValidationNel[ParseError[Json], Parsed[A]] =
        p.value.hcursor.get[A](k).fold(_ => retry(p.value.hcursor.downField(k)), some(_))
          .map(x => p.next(Op.TypeValue(x)))
          .toSuccessNel(ParseError(p, k, pErr[A]))
    },
    new CanParse[Option[A], Json] {
      def as(p: Parsed[Json]): ValidationNel[ParseError[Json], Parsed[Option[A]]] =
        p.value match {
          case Json.Null => p.next(Op.TypeValue(none[A])).successNel[ParseError[Json]]
          case v: Json => v.as[Option[A]]
                           .fold(_ => retry(v.hcursor), identity)
                           .map(a => p.next(Op.TypeValue(some(a))))
                           .toSuccessNel(ParseError(p, "_root_", iErr[A]))
        }

      def parse(k: String, p: Parsed[Json]): ValidationNel[ParseError[Json], Parsed[Option[A]]] =
        p.value.hcursor.get[Json](k).fold(
          _ => p.next(Op.TypeValue(none[A])).successNel[ParseError[Json]],
          v => as(p.next(v, Op.DownField(k))).leftMap(_.map(_.copy(key = k, error = pErr[A]))))
    })

  protected def gen[A: ClassTag: Decoder](fromStr: String => Option[A]): (CanParse[A, Json], CanParse[Option[A], Json], CanParse[List[A], Json], CanParse[Option[List[A]], Json]) = {
    val (a, oa) = gen0(_.as[String].toOption.flatMap(fromStr))
    val (la, loa) = gen0(_.as[List[String]].toOption.flatMap(_.traverse(fromStr)))
    (a, oa, la, loa)
  }

  implicit def cp[A: ClassTag: Decoder]: CanParse[A, Json] = gen0(_ => none[A])._1
  implicit def cpo[A: ClassTag: Decoder]: CanParse[Option[A], Json] = gen0(_ => none[A])._2
  implicit def cplo[A: ClassTag: Decoder]: CanParse[List[A], Json] = gen0(_ => none[List[A]])._1
  implicit def cpolo[A: ClassTag: Decoder]: CanParse[Option[List[A]], Json] = gen0(_ => none[List[A]])._2
}

object parsedinstances extends CatchAllInstance {
  implicit lazy val (pi: CanParse[Int, Json], pio: CanParse[Option[Int], Json],
                     pli: CanParse[List[Int], Json], plio: CanParse[Option[List[Int]], Json]) =
    gen(_.parseInt.toOption)

  implicit lazy val (pl: CanParse[Long, Json], plo: CanParse[Option[Long], Json],
                     pll: CanParse[List[Long], Json], pllo: CanParse[Option[List[Long]], Json]) =
    gen(_.parseLong.toOption)

  implicit lazy val (pd: CanParse[Double, Json], pdo: CanParse[Option[Double], Json],
                     pld: CanParse[List[Double], Json], pldo: CanParse[Option[List[Double]], Json]) =
    gen(_.parseDouble.toOption)

  implicit lazy val (pb: CanParse[Boolean, Json], pbo: CanParse[Option[Boolean], Json],
                     plb: CanParse[List[Boolean], Json], plbo: CanParse[Option[List[Boolean]], Json]) =
    gen(_.parseBoolean.toOption)
}
