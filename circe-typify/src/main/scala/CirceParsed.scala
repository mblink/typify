package circe.api.libs.json.typify

import io.circe.Json.{JArray, JBoolean, JNull, JNumber, JObject, JString}
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.id._
import scalaz.syntax.nel._
import scalaz.syntax.std.option._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.{\/, NonEmptyList, ValidationNel}
import typify.{CanParse, Parsed, ParseError}

private[this] object helper {
  def tpv[T](ct: String)(e: Either[DecodingFailure, T], kO: Option[String]=None): ValidationNel[ParseError, T] = e match {
    case Left(err) => ParseError(kO.getOrElse("_root_"), s"Could not be ${kO.map(_ => "parsed").getOrElse("interpreted")} as $ct:\n\t$err").failureNel[T]
    case Right(t) => t.successNel[ParseError]
  }
  def mk[T: ClassTag](ct: String)(implicit D: Decoder[T]) = new CanParse[T, Json] {
    def parse(k: String, jv: Json)(implicit T: ClassTag[T]): ValidationNel[ParseError, T] =
      tpv[T](ct)(jv.hcursor.downField(k).as[T], Some(k))

    def as(jv: Json)(implicit T: ClassTag[T]): ValidationNel[ParseError, T] = tpv[T](ct)(jv.as[T])
  }
}

trait CatchAllInstance {
  implicit def cp[T](implicit D: Decoder[T], ct: ClassTag[T]) =
    helper.mk[T](ct.toString)
}

trait CatchOptionInstance extends CatchAllInstance {
  implicit def parseO[T](implicit D: Decoder[T], ct: ClassTag[T]) =
    helper.mk[Option[T]](s"Option[${ct.toString}}")
}

object parsedinstances extends CatchOptionInstance {
  import helper.mk

  implicit lazy val pi = mk[Int]("Int")
  implicit lazy val pio = mk[Option[Int]]("Option[Int]")
  implicit lazy val pli = mk[List[Int]]("List[Int]")
  implicit lazy val plio = mk[Option[List[Int]]]("Option[List[Int]]")
  implicit lazy val pl = mk[Long]("Long")
  implicit lazy val plo = mk[Option[Long]]("Option[Long]")
  implicit lazy val pll = mk[List[Long]]("List[Long]")
  implicit lazy val pllo = mk[Option[List[Long]]]("Option[List[Long]]")
  implicit lazy val pd = mk[Double]("Double")
  implicit lazy val pdo = mk[Option[Double]]("Option[Double]")
  implicit lazy val pld = mk[List[Double]]("List[Double]")
  implicit lazy val pldo = mk[Option[List[Double]]]("Option[List[Double]]")
  implicit lazy val pb = mk[Boolean]("Boolean")
  implicit lazy val pbo = mk[Option[Boolean]]("Option[Boolean]")
}
