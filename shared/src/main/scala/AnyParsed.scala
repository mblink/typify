package typify

import scala.reflect.ClassTag
import scalaz.syntax.id._
import scalaz.syntax.nel._
import scalaz.syntax.std.option._
import scalaz.syntax.std.string._
import scalaz.syntax.validation._
import scalaz.ValidationNel

trait CatchAllInstance {

  implicit def cpt[T] = new CanParse[T, Any] {
    def as(jv: Any)(implicit ct: ClassTag[T]) = jv match {
      case s: T => s.successNel[ParseError]
      case _ => ParseError("_root_", s"Could not be interpreted as ${ct.toString}")
                  .failureNel[T]
    }

    def parse(k: String, a: Any)(implicit ct: ClassTag[T]) = a match {
      case m: Map[_, _] =>
        (m.asInstanceOf[Map[Any, Any]].get(k) \/>
          (ParseError(k, s"Could not be parsed as ${ct.toString}").wrapNel))
          .flatMap(as(_).disjunction).validation
      case _ => ParseError(k, s"Could not be parsed as ${ct.toString}").failureNel[T]
    }
  }

}

trait CatchOptionInstance extends CatchAllInstance {

  implicit def cpot[T: ClassTag] = new CanParse[Option[T], Any] {
    def as(a: Any)(implicit ct: ClassTag[Option[T]]) = a match {
      case Some(s) => s match {
        case t: T => Option(t).successNel[ParseError]
        case _ => None.successNel[ParseError]
      }
      case _ => None.successNel[ParseError]
    }

    def parse(k: String, a: Any)(implicit ct: ClassTag[Option[T]]) = a match {
      case m: Map[_, _] => m.asInstanceOf[Map[Any, Any]].get(k) match {
        case Some(x) => x match {
          case t: T => Some(t).successNel[ParseError]
          case _ => None.successNel[ParseError]
        }
        case _ => None.successNel[ParseError]
      }
      case _ => None.successNel[ParseError]
    }
  }
}

trait CatchAnyInstance extends CatchOptionInstance {

  lazy implicit val cpa = new CanParse[Any, Any] {
    def parse(k: String, a: Any)(implicit ct: ClassTag[Any]) = a match {
      case m: Map[_, _] =>
        m.asInstanceOf[Map[Any, Any]]
         .get(k).toSuccessNel(ParseError(k, "Could not be parsed as Any"))
      case _ => ParseError(k, "Could not be parsed as Any").failureNel[Any]
    }

    def as(a: Any)(implicit ct: ClassTag[Any]) = a.successNel[ParseError]
  }
}

object parsedany extends CatchOptionInstance {

  lazy implicit val cpojv = new CanParse[Option[Any], Any] {
    def as(a: Any)(implicit ct: ClassTag[Option[Any]]) = a match {
      case o: Option[Any] => o.successNel[ParseError]
      case x => Option(x).successNel[ParseError]
    }

    def parse(k: String, a: Any)(implicit ct: ClassTag[Option[Any]]) = a match {
      case m: Map[_, _] => {
        m.asInstanceOf[Map[Any, Any]].get(k) match {
          case Some(s) => s match {
            case o: Option[Any] => o.successNel[ParseError]
            case _ => None.successNel[ParseError]
          }
          case _ => None.successNel[ParseError]
        }
      }
      case _ => None.successNel[ParseError]
    }
  }
}

