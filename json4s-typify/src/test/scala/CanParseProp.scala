package typify

import org.json4s.{JObject, JValue, JNull}
import org.json4s.JsonDSL._
import org.json4s.typify.parsedinstances._
import org.scalacheck.Properties

object MakeJValue extends MakeParsed[JValue] {

  import implicits._

  def make[A](k: String, v: A)(implicit mp: MustParse[A]): JValue =
    mp match {
      case MPS => (k -> MPS(v))
      case MPOS => MPOS(v).map(x => (k -> x): JValue).getOrElse(JNull: JValue)
      case MPI => (k -> MPI(v))
      case MPOI => MPOI(v).map(x => (k -> x): JValue).getOrElse(JNull: JValue)
      case MPL => (k -> MPL(v))
      case MPOL => MPOL(v).map(x => (k -> x): JValue).getOrElse(JNull: JValue)
      case MPLI => (k -> MPLI(v))
      case MPOLI => MPOLI(v).map(x => (k -> x): JValue).getOrElse(JNull: JValue)
      case MPLS => (k -> MPLS(v))
      case MPOLS => MPOLS(v).map(x => (k -> x): JValue).getOrElse(JNull: JValue)
      case MPP => (k -> MPP(v))
      case MPOP => MPOP(v).map(x => (k -> v): JValue).getOrElse(JNull: JValue)
      case MPLP => (k -> MPLP(v))
    }
  def to[A](v: A)(implicit mp: MustParse[A]): JValue =
    mp match {
      case MPS => MPS(v)
      case MPOS => MPOS(v).map(x => (x: JValue)).getOrElse(JNull: JValue)
      case MPI => MPI(v)
      case MPOI => MPOI(v).map(x => (x: JValue)).getOrElse(JNull: JValue)
      case MPL => MPL(v)
      case MPOL => MPOL(v).map(x => (x: JValue)).getOrElse(JNull: JValue)
      case MPLI => MPLI(v)
      case MPOLI => MPOLI(v).map(x => (x: JValue)).getOrElse(JNull: JValue)
      case MPLS => MPLS(v)
      case MPOLS => MPOLS(v).map(x => (x: JValue)).getOrElse(JNull: JValue)
      case MPP => MPP(v)
      case MPOP => MPOP(v).getOrElse(JNull: JValue)
      case MPLP => MPLP(v)
    }
}

object Json4sCanParse extends Properties("json4s CanParse") {

  property("parses required types correctly") = new CanParseProp(MakeJValue).apply
}
