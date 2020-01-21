package circe.api.libs.json.typify

import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.validated._
import io.circe.{Decoder, Json}
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import typify._

trait CatchAllInstance {
  protected def gen[A: Decoder](retry: String => Option[A])(implicit ct: ClassTag[A]): (CanParse[A, Json], CanParse[Option[A], Json], CanParse[List[A], Json], CanParse[Option[List[A]], Json]) = {
    val cpa: CanParse[A, Json] = c =>
      c.focus.flatMap(j => j.as[Option[A]].toOption.flatten.orElse(j.as[String].toOption.flatMap(retry)))
        .toValidNel(ParseError(s"Could not be interpreted as $ct"))

    val cpla: CanParse[List[A], Json] = x => x.downArray match {
      case Cursor.Failed(_, _) => ParseError(s"Could not be interpreted as List[$ct]").invalidNel[List[A]]
      case c =>
        @tailrec def go(c: Cursor[Json], res: ValidatedNel[ParseError, List[A]]): ValidatedNel[ParseError, List[A]] =
          c match {
            case Cursor.Failed(_, _) => res
            case _ => go(c.right, (res, cpa(c)).mapN(_ :+ _))
          }

        go(c, List[A]().validNel[ParseError])
    }

    def opt[B](cp: CanParse[B, Json]): CanParse[Option[B], Json] =
      c => c.focus match {
        case Some(Json.Null) => None.validNel[ParseError]
        case Some(_) => cp(c).map(Some(_))
        case None => None.validNel[ParseError]
      }

    (cpa, opt(cpa), cpla, opt(cpla))
  }

  implicit def cp[A: ClassTag: Decoder]: CanParse[A, Json] = gen(_ => none[A])._1
  implicit def cpo[A: ClassTag: Decoder]: CanParse[Option[A], Json] = gen(_ => none[A])._2
  implicit def cplo[A: ClassTag: Decoder]: CanParse[List[A], Json] = gen(_ => none[A])._3
  implicit def cpolo[A: ClassTag: Decoder]: CanParse[Option[List[A]], Json] = gen(_ => none[A])._4
}

object parsedinstances extends CatchAllInstance {
  implicit val genJson: Generic[Json] = new Generic[Json] {
    def fromFields(fields: ListMap[String, Json]): Json = Json.fromFields(fields)
    def toFields(value: Json): Option[ListMap[String, Json]] = value.asObject.map(o => ListMap(o.toVector:_*))
    def fromValues(values: Vector[Json]): Json = Json.fromValues(values)
    def toValues(value: Json): Option[Vector[Json]] = value.asArray
  }

  implicit lazy val (pi: CanParse[Int, Json], pio: CanParse[Option[Int], Json],
                     pli: CanParse[List[Int], Json], plio: CanParse[Option[List[Int]], Json]) =
    gen(_.parseInt)

  implicit lazy val (pl: CanParse[Long, Json], plo: CanParse[Option[Long], Json],
                     pll: CanParse[List[Long], Json], pllo: CanParse[Option[List[Long]], Json]) =
    gen(_.parseLong)

  implicit lazy val (pd: CanParse[Double, Json], pdo: CanParse[Option[Double], Json],
                     pld: CanParse[List[Double], Json], pldo: CanParse[Option[List[Double]], Json]) =
    gen(_.parseDouble)

  implicit lazy val (pb: CanParse[Boolean, Json], pbo: CanParse[Option[Boolean], Json],
                     plb: CanParse[List[Boolean], Json], plbo: CanParse[Option[List[Boolean]], Json]) =
    gen(_.parseBoolean)
}
