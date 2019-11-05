package circe.api.libs.json.typify

import io.circe.{Decoder, DecodingFailure, Json}
import scala.reflect.ClassTag
import scalaz.syntax.validation._
import scalaz.ValidationNel
import typify.{CanParse, ParseError}

private[this] object helper {
  val bs = (kO: Option[String], ct: String, err: String) =>
    ParseError(kO.getOrElse("_root_"), s"Could not be ${kO.map(_ => "parsed").getOrElse("interpreted")} as $ct:\n\t$err")
  val hc = (kO: Option[String], jv: Json) => kO.map(jv.hcursor.downField(_)).getOrElse(jv.hcursor)
  def tpv[T](ct: String)(e: Either[DecodingFailure, T], kO: Option[String]=None): ValidationNel[ParseError, T] = e match {
    case Left(err) => bs(kO, ct, err.getMessage()).failureNel[T]
    case Right(t) => t.successNel[ParseError]
  }
  def mk[T](ct: String)(implicit D: Decoder[T]) = new CanParse[T, Json] {
    def parse(k: String, jv: Json)(implicit T: ClassTag[T]): ValidationNel[ParseError, T] =
      tpv[T](ct)(jv.hcursor.downField(k).as[T], Some(k))

    def as(jv: Json)(implicit T: ClassTag[T]): ValidationNel[ParseError, T] = tpv[T](ct)(jv.as[T])
  }

  def boolOpt(kO: Option[String], jv: Json)
  (implicit T: ClassTag[Boolean]): ValidationNel[ParseError, Boolean] =
    hc(kO, jv).as[Boolean].fold(
      _ => tpv("Boolean")(hc(kO, jv).as[String]).fold(_.failure[Boolean],
        (bStr: String) => bStr.toLowerCase match {
          case "true" => true.successNel[ParseError]
          case "false" => false.successNel[ParseError]
          case _ => bs(kO, T.toString, s"String $bStr could not be parsed as Boolean.").failureNel[Boolean]
        }),
      (_: Boolean).successNel[ParseError])

  def boolOOpt(kO: Option[String], jv: Json)
  (implicit T: ClassTag[Option[Boolean]]): ValidationNel[ParseError, Option[Boolean]] =
    hc(kO, jv).as[Option[Boolean]].fold(
      _ => tpv("Option[Boolean]")(hc(kO, jv).as[Option[String]]).fold(
        _.failure[Option[Boolean]],
        (bStr: Option[String]) => bStr.map(s => s.toLowerCase) match {
          case Some(s) if s == "true" => Some(true).successNel[ParseError]
          case Some(s) if s == "false" => Some(false).successNel[ParseError]
          case Some(_) => bs(kO, T.toString, s"String $bStr could not be parsed as Option[Boolean]").failureNel[Option[Boolean]]
          case None => None.successNel[ParseError]
        }),
      (_: Option[Boolean]).successNel[ParseError])

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
  import helper.{boolOpt, boolOOpt, mk}

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
  implicit lazy val pb = new CanParse[Boolean, Json] {
    def parse(k: String, jv: Json)(implicit T: ClassTag[Boolean]): ValidationNel[ParseError, Boolean] = boolOpt(Some(k), jv)
    def as(jv: Json)(implicit T: ClassTag[Boolean]): ValidationNel[ParseError, Boolean] = boolOpt(None, jv)
  }
  implicit lazy val pbo = new CanParse[Option[Boolean], Json] {
    def parse(k: String, jv: Json)(implicit T: ClassTag[Option[Boolean]]): ValidationNel[ParseError, Option[Boolean]] = boolOOpt(Some(k), jv)
    def as(jv: Json)(implicit T: ClassTag[Option[Boolean]]): ValidationNel[ParseError, Option[Boolean]] = boolOOpt(None, jv)
  }
}
