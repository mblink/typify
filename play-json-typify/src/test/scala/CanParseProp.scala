package typify

import play.api.libs.json.{JsValue, JsObject, JsString, JsDefined}
import play.api.libs.json.{JsNumber, JsUndefined, JsNull, Reads}
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
      case MPP => JsObject(Seq(k -> v))
      case MPOP => MPOP(v).map(x => JsObject(Seq(k -> x))).getOrElse(JsNull)
    }
  def to[A](v: A)(implicit mp: MustParse[A]): JsValue =
    mp match {
      case MPS => JsString(v)
      case MPOS => MPOS(v).map(JsString(_)).getOrElse(JsNull)
      case MPI => JsNumber(v)
      case MPOI => MPOI(v).map(JsNumber(_)).getOrElse(JsNull)
      case MPL => JsNumber(v)
      case MPOL => MPOL(v).map(JsNumber(_)).getOrElse(JsNull)
      case MPP => MPP(v)
      case MPOP => MPOP(v).getOrElse(JsNull)
    }
}

object PlayJsonCanParse extends Properties("playjson CanParse") {

  property("parses required types correctly") = new CanParseProp(MakeJsValue).apply
}

