package circe.api.libs.json.typify

import io.circe.{ACursor, Decoder, Json}
import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import typify.{CanParse, Op, Validated, ValidatedHelper}

trait CatchAllInstance extends ValidatedHelper {
  private def gen0[A: ClassTag: Decoder](retry: ACursor => Option[A]): (CanParse[A, Json], CanParse[Option[A], Json]) =
    (new CanParse[A, Json] {
      def as(j: Json): Validated[A] =
        Validated(ops => j.as[A].fold(_ => retry(j.hcursor), some(_))
          .fold(Op.typedValueError[A](ops, none[A]))(Op.typedValue(_)))

      def parse(k: String, j: Json): Validated[A] =
        Validated(ops => j.hcursor.downField(k).focus.fold(
          Op.downFieldError[Json](ops, k))(Op.downField(_, k))).flatMap(as(_))
    },
    new CanParse[Option[A], Json] {
      def as(j: Json): Validated[Option[A]] =
        Validated(ops => j match {
          case Json.Null => Op.typedValue(none[A])
          case v: Json => v.as[Option[A]]
                           .fold(_ => retry(v.hcursor), identity)
                           .fold(Op.typedValueError[Option[A]](ops, none[A]))(
                            a => Op.typedValue(some(a)))
        })

      def parse(k: String, j: Json): Validated[Option[A]] =
        Validated(_ => Op.downField(j.hcursor.get[Json](k).toOption
          .getOrElse(Json.Null), k)).flatMap(as(_))
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
