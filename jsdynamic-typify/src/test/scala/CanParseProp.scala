package typify

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.typify.parsedinstances._
import org.scalacheck.Properties

object MakeJsDynamic extends MakeParsed[js.Dynamic] {

  import implicits._

  def make[A](k: String, v: A)(implicit mp: MustParse[A]): js.Dynamic =
    mp match {
      case MPS => literal(k -> v)
      case MPOS => MPOS(v).map(x => literal(k -> x)).getOrElse(().asInstanceOf[js.Dynamic])
      case MPI => literal(k -> v)
      case MPOI => MPOI(v).map(x => literal(k -> x)).getOrElse(().asInstanceOf[js.Dynamic])
      case MPL => literal(k -> v)
      case MPOL => MPOL(v).map(x => literal(k -> x)).getOrElse(().asInstanceOf[js.Dynamic])
      case MPP => literal(k -> v)
      case MPOP => MPOP(v).map(x => literal(k -> x)).getOrElse(().asInstanceOf[js.Dynamic])
    }
  def to[A](v: A)(implicit mp: MustParse[A]): js.Dynamic =
    mp match {
      case MPS => v.asInstanceOf[js.Dynamic]
      case MPOS => MPOS(v).map(_.asInstanceOf[js.Dynamic])
                    .getOrElse(().asInstanceOf[js.Dynamic])
      case MPI => v.asInstanceOf[js.Dynamic]
      case MPOI => MPOI(v).map(_.asInstanceOf[js.Dynamic])
                    .getOrElse(().asInstanceOf[js.Dynamic])
      case MPL => v.asInstanceOf[js.Dynamic]
      case MPOL => MPOL(v).map(_.asInstanceOf[js.Dynamic])
                    .getOrElse(().asInstanceOf[js.Dynamic])
      case MPP => v.asInstanceOf[js.Dynamic]
      case MPOP => MPOP(v).map(_.asInstanceOf[js.Dynamic])
                    .getOrElse(().asInstanceOf[js.Dynamic])
    }
}

@JSExport
object jsDynamicCanParse extends Properties("playjson CanParse") {

  val prop = new CanParseProp(MakeJsDynamic)

  // scalajs and long have an "opaque" relationship, see here
  // http://stackoverflow.com/questions/27821841/working-with-opaque-types-char-and-long
  property("parses required types correctly") =
    prop.string && prop.int && prop.recursive
}
