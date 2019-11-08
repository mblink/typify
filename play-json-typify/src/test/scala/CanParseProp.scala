package typify

import play.api.libs.json.{Json, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}
import play.api.libs.json.typify.parsedinstances._
import org.scalacheck.Properties

object MakeJsValue extends MakeParsed[JsValue] {
  import implicits._

  def make[A](k: String, v: A)(implicit mp: MustParse[A]): JsValue =
    mp match {
      case MPS => JsObject(Seq(k -> JsString(v)))
      case MPOS => MPOS(v).map(x => JsObject(Seq(k -> JsString(x)))).getOrElse(JsNull)
      case MPI => JsObject(Seq(k -> JsNumber(v)))
      case MPOI => MPOI(v).map(x => JsObject(Seq(k -> JsNumber(x)))).getOrElse(JsNull)
      case MPL => JsObject(Seq(k -> JsNumber(v)))
      case MPOL => MPOL(v).map(x => JsObject(Seq(k -> JsNumber(x)))).getOrElse(JsNull)
      case MPD => JsObject(Seq(k -> JsNumber(v)))
      case MPOD => MPOD(v).map(x => JsObject(Seq(k -> JsNumber(x)))).getOrElse(JsNull)
      case MPB => JsObject(Seq(k -> JsBoolean(v)))
      case MPOB => MPOB(v).map(x => JsObject(Seq(k -> JsBoolean(x)))).getOrElse(JsNull)
      case MPLI => JsObject(Seq(k -> Json.toJson(v)))
      case MPOLI => MPOLI(v).map(x => JsObject(Seq(k -> Json.toJson(x)))).getOrElse(JsNull)
      case MPLS => JsObject(Seq(k -> Json.toJson(v)))
      case MPOLS => MPOLS(v).map(x => JsObject(Seq(k -> Json.toJson(x)))).getOrElse(JsNull)
      case MPP => JsObject(Seq(k -> v))
      case MPOP => MPOP(v).map(x => JsObject(Seq(k -> x))).getOrElse(JsNull)
      case MPLP => JsObject(Seq(k -> Json.toJson(v)))
    }

  def to[A](v: A)(implicit mp: MustParse[A]): JsValue =
    mp match {
      case MPS => JsString(v)
      case MPOS => MPOS(v).map(JsString(_)).getOrElse(JsNull)
      case MPI => JsNumber(v)
      case MPOI => MPOI(v).map(JsNumber(_)).getOrElse(JsNull)
      case MPL => JsNumber(v)
      case MPOL => MPOL(v).map(JsNumber(_)).getOrElse(JsNull)
      case MPD => JsNumber(v)
      case MPOD => MPOD(v).map(JsNumber(_)).getOrElse(JsNull)
      case MPB => JsBoolean(v)
      case MPOB => MPOB(v).map(JsBoolean(_)).getOrElse(JsNull)
      case MPLI => Json.toJson(v)
      case MPOLI => MPOLI(v).map(Json.toJson(_)).getOrElse(JsNull)
      case MPLS => Json.toJson(v)
      case MPOLS => MPOLS(v).map(Json.toJson(_)).getOrElse(JsNull)
      case MPP => MPP(v)
      case MPOP => MPOP(v).getOrElse(JsNull)
      case MPLP => Json.toJson(v)
    }
}

object PlayJsonCanParse extends Properties("CanParse") {
  include(new CanParseProp(MakeJsValue).props("playjson"))
}
