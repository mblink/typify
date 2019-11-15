package typify

import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

trait CatchAllInstance {
  protected def parseMap(x: Any): Option[Map[Any, Any]] =
    Some(x).collect { case m: Map[_, _] => m.asInstanceOf[Map[Any, Any]] }

  protected trait ParseRequired[T] extends CanParse[T, Any] {
    def parse(k: String, x: Any): ParsedValidated[T] =
      for {
        m <- parseMap(x).fold(Op.typeValueError(none[Map[Any, Any]]))(Op.typeValue(_))
        v <- m.get(k).fold(Op.downFieldError[Any](k))(Op.downField(_, k))
        t <- as(v)
      } yield t
  }

  protected trait ParseOptional[T] extends CanParse[Option[T], Any] {
    def parse(k: String, x: Any): ParsedValidated[Option[T]] =
      for {
        m <- Op.typeValue(parseMap(x).getOrElse(Map()))
        vo <- Op.downField(m.get(k), k)
        t <- vo.fold(Op.typeValue(none[T]))(as(_))
      } yield t
  }

  implicit def cpt[T](implicit ct: ClassTag[T]) = new ParseRequired[T] {
    def as(x: Any): ParsedValidated[T] =
      x match {
        case t: T => Op.typeValue(t)
        case _ => Op.typeValueError[T](none[T])
      }
  }
}

trait CatchOptionInstance extends CatchAllInstance {
  implicit def cpot[T: ClassTag] = new ParseOptional[T] {
    def as(x: Any): ParsedValidated[Option[T]] =
      Op.typeValue(x match {
        case o@Some(_) => o.collect { case t: T => t }
        case t: T => Option(t)
        case _ => none[T]
      })
  }

  implicit def cplt[T: ClassTag](implicit cpt: CanParse[T, Any]) = new ParseRequired[List[T]] {
    def as(x: Any): ParsedValidated[List[T]] =
      x match {
        case l: List[Any] => l.zipWithIndex.traverse(t => Op.arrayIndex(t._1, t._2).flatMap(cpt.as(_)))
        case _ => Op.typeValueError(none[List[T]])
      }
  }
}

object parsedany extends CatchOptionInstance {
  lazy implicit val cpoany = new ParseOptional[Any] {
    def as(x: Any): ParsedValidated[Option[Any]] =
      Op.typeValue(x match {
        case o: Option[Any] => o
        case y => Option(y)
      })
  }

  import scalaz.syntax.std.boolean._
  import scalaz.syntax.validation._
  import shapeless.HNil
  import shapeless.syntax.singleton._
  import typify.Validated.syntax._

  case class Fail(reason: String, ops: Vector[Op])

  val tp = new Typify[Fail, Any]

  implicit val parse2Error = (_: Parsed[Any], pe: ParseError) => Fail(pe.error, pe.ops :+ pe.failedOp)

  val checkEmail = Typify.validate((_: String).successNel[Fail].ensureV(Fail("Email is invalid", _))(_.contains("@")))

  val checkAge = Typify.validate((_: Int).successNel[Fail].ensureV(Fail("Too young", _))(_ > 21))

  val checkSessId = Typify.optional((_: Int).successNel[Fail].ensureV(Fail("Invalid session id", _))(_ > 3000))

  val checkStrs = Typify.validate((_: List[String]).zipWithIndex.traverse { case (s, i) =>
    (s.length > 0).fold[Validated[Fail, String]](
      Op.arrayIndexL[Fail](s, i),
      Validated(ops => Fail("Empty string", ops).failureNel[(Vector[Op], String)]))
  })

  val checkPerson = 'email ->> checkEmail :: 'age ->> checkAge :: 'session ->> checkSessId :: 'strings ->> checkStrs :: HNil

  val passes: Any = Map("thing" -> Map("other" -> Map[Any, Any](
    "email" -> "foo@bar", "age" -> 22, "session" -> 77777, 3 -> "junk",
    "strings" -> List("a", "b", "c"))))
  val fails: Any = Map("thing" -> Map("other" -> Map[Any, Any](
    "email" -> "fake email", "age" -> "foo", "session" -> 77777, 3 -> "junk",
    "strings" -> List[Any]("a", "b", 2))))

  import tp.syntax._

  val passed = Parsed(passes, Vector("thing", "other")).parse(checkPerson)
  val failed = Parsed(fails, Vector("thing", "other")).parse(checkPerson)
}
