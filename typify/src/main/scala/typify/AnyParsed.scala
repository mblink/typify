package typify

import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.apply._
import cats.syntax.validated._
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

trait CatchAllInstance {
  implicit def cpt[T](implicit ct: ClassTag[T]) = new CanParse[T, Any] {
    def apply(x: Cursor[Any]): ValidatedNel[ParseError, T] =
      x.focus match {
        case Some(t: T) => t.validNel[ParseError]
        case _ => ParseError(s"Could not be interpreted as $ct").invalidNel[T]
      }
  }
}

trait CatchOptionInstance extends CatchAllInstance {
  implicit def cpot[T: ClassTag] = new CanParse[Option[T], Any] {
    def apply(x: Cursor[Any]): ValidatedNel[ParseError, Option[T]] =
      (x.focus match {
        case Some(Some(t: T)) => Option(t)
        case _ => Option.empty[T]
      }).validNel[ParseError]
  }

  implicit def cplt[T](implicit ct: ClassTag[T], cpt: CanParse[T, Any]) = new CanParse[List[T], Any] {
    @tailrec def go(c: Cursor[Any], res: ValidatedNel[ParseError, List[T]]): ValidatedNel[ParseError, List[T]] =
      c match {
        case Cursor.Failed(_, _) => res
        case _ => go(c.right, (res, cpt(c)).mapN(_ :+ _))
      }

    def apply(x: Cursor[Any]): ValidatedNel[ParseError, List[T]] = x.downArray match {
      case Cursor.Failed(_, _) => ParseError(s"Could not be interpreted as List[$ct]").invalidNel[List[T]]
      case c => go(c, List[T]().validNel[ParseError])
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
    def apply(x: Cursor[Any]): ValidatedNel[ParseError, Option[Any]] =
      (x.focus match {
        case Some(o: Option[Any]) => o
        case Some(x) => Option(x)
        case _ => None
      }).validNel[ParseError]
  }

  import cats.instances.list._
  import cats.syntax.traverse._
  import shapeless.HNil
  import shapeless.syntax.singleton._

  case class Fail(reason: String, ops: Vector[CursorOp])

  val tp = new Typify[Fail, Any]

  implicit val parse2Error = (c: Cursor[Any], pe: ParseError) => Fail(pe.message, c.history)

  val checkEmail = Typify.validate((_: String, s: String, c: Cursor[Any]) =>
    s.validNel[Fail].ensure(NonEmptyList.of(Fail("Email is invalid", c.history)))(_.contains("@")))

  val checkAge = Typify.validate((_: String, i: Int, c: Cursor[Any]) =>
    i.validNel[Fail].ensure(NonEmptyList.of(Fail("Too young", c.history)))(_ > 21))

  val checkSessId = Typify.optional((_: String, i: Int, c: Cursor[Any]) =>
    i.validNel[Fail].ensure(NonEmptyList.of(Fail("Invalid session id", c.history)))(_ > 3000))

  val checkStrs = Typify.validate((_: String, l: List[String], c: Cursor[Any]) =>
    l.traverse(_.validNel[Fail].ensure(NonEmptyList.of(Fail("Empty string", c.history)))(_.nonEmpty)))

  val checkPerson = 'email ->> checkEmail :: 'age ->> checkAge :: 'session ->> checkSessId :: 'strings ->> checkStrs :: HNil

  val passes: Any = Map("thing" -> Map("other" -> Map[Any, Any](
    "email" -> "foo@bar", "age" -> 22, "session" -> 77777, 3 -> "junk",
    "strings" -> List("a", "b", "c"))))
  val fails: Any = Map("thing" -> Map("other" -> Map[Any, Any](
    "email" -> "fake email", "age" -> "foo", "session" -> 77777, 3 -> "junk",
    "strings" -> List[Any]("a", "b", 2))))

  import tp.syntax._

  val passed = Parsed.top(passes, Vector("thing", "other")).parse(checkPerson)
  val failed = Parsed.top(fails, Vector("thing", "other")).parse(checkPerson)
}
