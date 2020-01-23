package typify

import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.validated._
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

trait CatchAllInstance {
  implicit def cpt[T](implicit ct: ClassTag[T]) = new CanParse[T, Any] {
    def apply(x: Cursor[Any]): ValidatedNel[ParseError[Any], T] =
      x.focus match {
        case Some(t: T) => t.validNel[ParseError[Any]]
        case _ => ParseError(x, s"Could not be interpreted as $ct").invalidNel[T]
      }
  }
}

trait CatchOptionInstance extends CatchAllInstance {
  implicit def cpot[T: ClassTag] = new CanParse[Option[T], Any] {
    def apply(x: Cursor[Any]): ValidatedNel[ParseError[Any], Option[T]] =
      (x.focus match {
        case Some(Some(t: T)) => Option(t)
        case _ => Option.empty[T]
      }).validNel[ParseError[Any]]
  }

  implicit def cplt[T](implicit ct: ClassTag[T], cpt: CanParse[T, Any]) = new CanParse[List[T], Any] {
    @tailrec def go(c: Cursor[Any], res: ValidatedNel[ParseError[Any], List[T]]): ValidatedNel[ParseError[Any], List[T]] =
      c match {
        case Cursor.Failed(_, _) => res
        case _ => go(c.right, (res, cpt(c)).mapN(_ :+ _))
      }

    def apply(x: Cursor[Any]): ValidatedNel[ParseError[Any], List[T]] = x.downArray match {
      case f @ Cursor.Failed(_, _) => ParseError(f, s"Could not be interpreted as List[$ct]").invalidNel[List[T]]
      case c => go(c, List[T]().validNel[ParseError[Any]])
    }
  }
}

object parsedany extends CatchOptionInstance {
  implicit val genericAny: Generic[Any] = new Generic[Any] {
    def toValues(value: Any): Option[Vector[Any]] =
      Some(value).collect { case i: Iterable[_] => i.toVector }

    def fromValues(values: Vector[Any]): Any =
      values

    def toFields(value: Any): Option[ListMap[String, Any]] =
      Some(value).collect { case m: Map[String, _] @unchecked => ListMap(m.toSeq:_*) }

    def fromFields(fields: ListMap[String, Any]): Any =
      fields
  }

  lazy implicit val cpoany = new CanParse[Option[Any], Any] {
    def apply(x: Cursor[Any]): ValidatedNel[ParseError[Any], Option[Any]] =
      (x.focus match {
        case Some(o: Option[Any]) => o
        case Some(x) => Option(x)
        case _ => None
      }).validNel[ParseError[Any]]
  }
}
