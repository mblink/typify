package circe.api.libs.json.typify

import io.circe.{Decoder, Json}
import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import typify.{CanParse, Op, ParsedValidated}

trait CatchAllInstance {
  implicit class JsonOps(j: Json) {
    def asO[A: Decoder](f: String => Option[A]): Option[A] =
      j.as[Option[A]].toOption.flatten.orElse(j.as[String].toOption.flatMap(f))
  }

  protected def gen[A: ClassTag: Decoder](retry: String => Option[A]): (CanParse[A, Json], CanParse[Option[A], Json], CanParse[List[A], Json], CanParse[Option[List[A]], Json]) =
    (new CanParse[A, Json] {
      def as(j: Json): ParsedValidated[A] =
        j.asO(retry).fold(Op.typeValueError(none[A]))(Op.typeValue(_))

      def parse(k: String, j: Json): ParsedValidated[A] =
        j.hcursor.downField(k).focus.fold(Op.downFieldError[Json](k))(Op.downField(_, k)).flatMap(as(_))
    },
    new CanParse[Option[A], Json] {
      def as(j: Json): ParsedValidated[Option[A]] =
        j match {
          case Json.Null => Op.typeValue(none[A])
          case v => v.asO(retry).fold(Op.typeValueError[Option[A]](None))(a => Op.typeValue(some(a)))
        }

      def parse(k: String, j: Json): ParsedValidated[Option[A]] =
        Op.downField(j.hcursor.get[Json](k).toOption.getOrElse(Json.Null), k).flatMap(as(_))
    },
    new CanParse[List[A], Json] {
      def as(j: Json): ParsedValidated[List[A]] =
        for {
          js <- j.asArray.fold(Op.typeValueError(none[List[Json]]))(v => Op.typeValue(v.toList))
          res <- js.zipWithIndex.traverse(t => Op.arrayIndex(t._1, t._2).flatMap(
            v => v.asO(retry).fold(Op.typeValueError(none[A]))(Op.typeValue(_))))
        } yield res

      def parse(k: String, j: Json): ParsedValidated[List[A]] =
        j.hcursor.downField(k).focus.fold(Op.downFieldError[Json](k))(Op.downField(_, k)).flatMap(as(_))
    },
    new CanParse[Option[List[A]], Json] {
      def as(j: Json): ParsedValidated[Option[List[A]]] =
        j.asArray.fold(Op.typeValue(none[List[A]]))(_.toList.zipWithIndex.traverse(t =>
          Op.arrayIndex(t._1, t._2).flatMap(v => v.asO(retry).fold(Op.typeValueError(none[A]))(Op.typeValue(_)))).map(some(_)))

      def parse(k: String, j: Json): ParsedValidated[Option[List[A]]] =
        Op.downField(j.hcursor.get[Json](k).toOption.getOrElse(Json.Null), k).flatMap(as(_))
    })

  implicit def cp[A: ClassTag: Decoder]: CanParse[A, Json] = gen(_ => none[A])._1
  implicit def cpo[A: ClassTag: Decoder]: CanParse[Option[A], Json] = gen(_ => none[A])._2
  implicit def cplo[A: ClassTag: Decoder]: CanParse[List[A], Json] = gen(_ => none[A])._3
  implicit def cpolo[A: ClassTag: Decoder]: CanParse[Option[List[A]], Json] = gen(_ => none[A])._4
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
