package typify

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.typify.parsedinstances._
import org.scalacheck.{Gen, Properties}

import scalaz.syntax.id._

object MakeJsDynamic extends MakeParsed[js.Dynamic] {

  import implicits._
  import js.JSConverters._

  def none = Gen.oneOf(().asInstanceOf[js.Dynamic], null.asInstanceOf[js.Dynamic])
                .sample.getOrElse(().asInstanceOf[js.Dynamic])

  def make[A](k: String, v: A)(implicit mp: MustParse[A]): js.Dynamic =
    mp match {
      case MPS => literal(k -> v)
      case MPOS => MPOS(v).map(x => literal(k -> x)).getOrElse(none)
      case MPI => literal(k -> v)
      case MPOI => MPOI(v).map(x => literal(k -> x)).getOrElse(none)
      case MPL => literal(k -> v)
      case MPOL => MPOL(v).map(x => literal(k -> x)).getOrElse(none)
      case MPLI => literal(k -> v.toSeq.toJSArray)
      case MPOLI => MPOLI(v).map(x => literal(k -> x.toSeq.toJSArray)).getOrElse(none)
      case MPLS => literal(k -> v.toSeq.toJSArray)
      case MPOLS => MPOLS(v).map(x => literal(k -> x.toSeq.toJSArray)).getOrElse(none)
      case MPP => literal(k -> v)
      case MPOP => MPOP(v).map(x => literal(k -> x)).getOrElse(none)
      case MPLP => literal(k -> v.toSeq.toJSArray)
    }

  def to[A](v: A)(implicit mp: MustParse[A]): js.Dynamic =
    mp match {
      case MPS => v.asInstanceOf[js.Dynamic]
      case MPOS => MPOS(v).map(_.asInstanceOf[js.Dynamic]).getOrElse(none)
      case MPI => v.asInstanceOf[js.Dynamic]
      case MPOI => MPOI(v).map(_.asInstanceOf[js.Dynamic]).getOrElse(none)
      case MPL => v.asInstanceOf[js.Dynamic]
      case MPOL => MPOL(v).map(_.asInstanceOf[js.Dynamic]).getOrElse(none)
      case MPLI => MPLI(v).toSeq.toJSArray.asInstanceOf[js.Dynamic]
      case MPOLI => MPOLI(v).map(_.toSeq.toJSArray.asInstanceOf[js.Dynamic]).getOrElse(none)
      case MPLS => MPLS(v).toSeq.toJSArray.asInstanceOf[js.Dynamic]
      case MPOLS => MPOLS(v).map(_.toSeq.toJSArray.asInstanceOf[js.Dynamic]).getOrElse(none)
      case MPP => v.asInstanceOf[js.Dynamic]
      case MPOP => MPOP(v).map(_.asInstanceOf[js.Dynamic]).getOrElse(none)
      case MPLP => MPLP(v).toSeq.toJSArray.asInstanceOf[js.Dynamic]
    }
}

@JSExport
object jsDynamicCanParse extends Properties("js.Dynamic CanParse") {

  val prop = new CanParseProp(MakeJsDynamic)

  // scalajs and long have an "opaque" relationship, see here
  // http://stackoverflow.com/questions/27821841/working-with-opaque-types-char-and-long
  property("parses required types correctly") =
    prop.string && prop.int && prop.list && prop.recursive
}
