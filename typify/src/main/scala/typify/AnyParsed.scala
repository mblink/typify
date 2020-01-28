package typify

import cats.data.ValidatedNel
import cats.syntax.validated._
import org.scalacheck.Arbitrary
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import shapeless.{::, HList, Poly2}
import shapeless.labelled.{field, FieldType}

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

  implicit def cplt[T](implicit ct: ClassTag[T], cpt: CanParse[T, Any]): CanParse[List[T], Any] = parseList(_: Cursor[Any])(
    f => ParseError(f, s"Could not be interpreted as List[$ct]").invalidNel[List[T]],
    cpt(_))
}

trait GenericAnyInstance {
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
}

object parsedany extends CatchOptionInstance {
  lazy implicit val cpoany = new CanParse[Option[Any], Any] {
    def apply(x: Cursor[Any]): ValidatedNel[ParseError[Any], Option[Any]] =
      (x.focus match {
        case Some(o: Option[Any]) => o
        case _ => None
      }).validNel[ParseError[Any]]
  }

  class TypifyAny[L, P](val t: Typify[L, P]) {
    import t._

    object genPV extends Poly2 {
      implicit def labelledPV[O <: HList, K <: Symbol, A](implicit arb: Arbitrary[A]): Case.Aux[FieldType[K, PV[A]], Arbitrary[O], Arbitrary[FieldType[K, A] :: O]] =
        at((_, acc) => Arbitrary(acc.arbitrary.flatMap(o => arb.arbitrary.map(a => field[K](a) :: o))))

      implicit def labelledKPV[O <: HList, K <: Symbol, A](implicit arb: Arbitrary[A]): Case.Aux[FieldType[K, KPV[A]], Arbitrary[O], Arbitrary[FieldType[K, A] :: O]] =
        at((_, acc) => Arbitrary(acc.arbitrary.flatMap(o => arb.arbitrary.map(a => field[K](a) :: o))))
    }
  }

  import cats.data.NonEmptyList
  import cats.instances.list._
  import cats.syntax.traverse._
  import shapeless.HNil
  import shapeless.syntax.singleton._

  case class Fail(reason: String, ops: Vector[CursorOp])

  val tp = new Typify[Fail, Any]

  implicit val parse2Error = (pe: ParseError[Any]) => Fail(pe.message, pe.cursor.history)

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

  val passed = Cursor.at(passes, Vector("thing", "other")).parse(checkPerson)
  val failed = Cursor.at(fails, Vector("thing", "other")).parse(checkPerson)

  val passedO = Cursor.at(Map("thing" -> Map("other" -> Map())): Any, Vector("thing", "other", "foo")).parseOption(checkPerson)

  val ta = new TypifyAny(tp)

  val genPerson = checkPerson.foldRight(Arbitrary(org.scalacheck.Gen.const(HNil)))(ta.genPV)
}
