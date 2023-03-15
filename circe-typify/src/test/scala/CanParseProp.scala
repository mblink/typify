package typify

import io.circe.Json
import io.circe.parsedinstances._
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
      case MPP => Map[String, Json](k -> v).asJson
      case MPOP => Map[String, Option[Json]](k -> v).asJson
      case MPLP => Map[String, List[Json]](k -> v).asJson
    })

  def to[A](v: A)(implicit mp: MustParse[A]): Cursor[Json] =
    Cursor.top(mp match {
      case MPS => (v: String).asJson
      case MPOS => (v: Option[String]).asJson
      case MPI => (v: Int).asJson
      case MPOI => (v: Option[Int]).asJson
      case MPL => (v: Long).asJson
      case MPOL => (v: Option[Long]).asJson
      case MPD => (v: Double).asJson
      case MPOD => (v: Option[Double]).asJson
      case MPB => (v: Boolean).asJson
      case MPOB => (v: Option[Boolean]).asJson
      case MPLI => (v: List[Int]).asJson
      case MPOLI => (v: Option[List[Int]]).asJson
      case MPLS => (v: List[String]).asJson
      case MPOLS => (v: Option[List[String]]).asJson
      case MPP => (v: Json).asJson
      case MPOP => (v: Option[Json]).asJson
      case MPLP => (v: List[Json]).asJson
    })
}

object CirceCanParse extends Properties("CanParse") {
  include(new CanParseProp(MakeJson).props("circe"))
}
