package typify

import scala.reflect.ClassTag
import scalaz.std.option._
import scalaz.syntax.validation._

trait CatchAllInstance {
  protected def parseMap(p: Parsed[Any]): Option[Map[Any, Any]] =
    Some(p.value).collect { case m: Map[_, _] => m.asInstanceOf[Map[Any, Any]] }

  implicit def cpt[T](implicit ct: ClassTag[T]) = new CanParse[T, Any] {
    def as(p: Parsed[Any]) =
      p.value match {
        case t: T => p.next(Op.TypeValue(t)).successNel[ParseError[Any]]
        case _ => ParseError(p, "_root_", s"Could not be interpreted as $ct").failureNel[Parsed[T]]
      }

    def parse(k: String, p: Parsed[Any]) = {
      val err = ParseError(p, k, s"Could not be parsed as $ct").failureNel[Parsed[T]]
      parseMap(p).fold(err)(_.get(k).fold(err)(x =>
        as(p.next(x, Op.DownField(k))).leftMap(_.map(_.copy(key = k)))))
    }
  }
}

trait CatchOptionInstance extends CatchAllInstance {
  implicit def cpot[T: ClassTag] = new CanParse[Option[T], Any] {
    def as(p: Parsed[Any]) =
      (p.value match {
        case o@Some(_) => p.next(Op.TypeValue(o.collect { case t: T => t }))
        case t: T => p.next(Op.TypeValue(Option(t)))
        case _ => p.next(Op.TypeValue(none[T]))
      }).successNel[ParseError[Any]]

    def parse(k: String, p: Parsed[Any]) =
      parseMap(p).flatMap(_.get(k)).fold(p.next(Op.TypeValue(none[T])).successNel[ParseError[Any]])(x =>
        as(p.next(x, Op.DownField(k))).leftMap(_.map(_.copy(key = k))))
  }
}

object parsedany extends CatchOptionInstance {
  lazy implicit val cpoany = new CanParse[Option[Any], Any] {
    def as(p: Parsed[Any]) =
      p.next(Op.TypeValue(p.value match {
        case o: Option[Any] => o
        case x => Option(x)
      })).successNel[ParseError[Any]]

    def parse(k: String, p: Parsed[Any]) =
      parseMap(p).flatMap(_.get(k)).fold(p.next(Op.TypeValue(none[Any])).successNel[ParseError[Any]])(x =>
        as(p.next(x, Op.DownField(k))).leftMap(_.map(_.copy(key = k))))
  }
}
