package typify

import circe.api.libs.json.typify.parsedinstances._
import io.circe.Json
import io.circe.syntax._
import org.scalacheck.Properties

object MakeJson extends MakeParsed[Json] {
  import implicits._

  def make[A](k: String, v: A)(implicit mp: MustParse[A]): Cursor[Json] =
    Cursor.top(mp match {
      case MPS => Map[String, String](k -> v).asJson
      case MPOS => Map[String, Option[String]](k -> v).asJson
      case MPI => Map[String, Int](k -> v).asJson
      case MPOI => Map[String, Option[Int]](k -> v).asJson
      case MPL => Map[String, Long](k -> v).asJson
      case MPOL => Map[String, Option[Long]](k -> v).asJson
      case MPD => Map[String, Double](k -> v).asJson
      case MPOD => Map[String, Option[Double]](k -> v).asJson
      case MPB => Map[String, Boolean](k -> v).asJson
      case MPOB => Map[String, Option[Boolean]](k -> v).asJson
      case MPLI => Map[String, List[Int]](k -> v).asJson
      case MPOLI => Map[String, Option[List[Int]]](k -> v).asJson
      case MPLS => Map[String, List[String]](k -> v).asJson
      case MPOLS => Map[String, Option[List[String]]](k -> v).asJson
      case MPP => Map[String, A](k -> v).asJson
      case MPOP => Map[String, Option[A]](k -> Some(v)).asJson
      case MPLP => Map[String, List[A]](k -> List(v)).asJson
    })

  def to[A](v: A)(implicit mp: MustParse[A]): Cursor[Json] =
    Cursor.top(mp match {
      case MPS => v.asJson
      case MPOS => v.asJson
      case MPI => v.asJson
      case MPOI => v.asJson
      case MPL => v.asJson
      case MPOL => v.asJson
      case MPD => v.asJson
      case MPOD => v.asJson
      case MPB => v.asJson
      case MPOB => v.asJson
      case MPLI => v.asJson
      case MPOLI => v.asJson
      case MPLS => v.asJson
      case MPOLS => v.asJson
      case MPP => v.asJson
      case MPOP => v.asJson
      case MPLP => v.asJson
    })
}

object CirceCanParse extends Properties("CanParse") {
  include(new CanParseProp(MakeJson).props("circe"))
}
