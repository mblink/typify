package typify

import scala.reflect.ClassTag
import scalaz.std.option._

trait CatchAllInstance {
  protected def parseMap(x: Any): Option[Map[Any, Any]] =
    Some(x).collect { case m: Map[_, _] => m.asInstanceOf[Map[Any, Any]] }

  implicit def cpt[T](implicit ct: ClassTag[T]) = new CanParse[T, Any] {
    def as(x: Any): ParsedValidated[T] =
      x match {
        case t: T => Op.typeValue(t)
        case _ => Op.typeValueError[T](none[T])
      }

    def parse(k: String, x: Any): ParsedValidated[T] =
      for {
        m <- parseMap(x).fold(Op.typeValueError[Map[Any, Any]](none[Map[Any, Any]]))(Op.typeValue(_))
        v <- m.get(k).fold(Op.downFieldError[Any](k))(Op.downField(_, k))
        t <- as(v)
      } yield t
  }
}

trait CatchOptionInstance extends CatchAllInstance {
  implicit def cpot[T: ClassTag] = new CanParse[Option[T], Any] {
    def as(x: Any): ParsedValidated[Option[T]] =
      Op.typeValue(x match {
        case o@Some(_) => o.collect { case t: T => t }
        case t: T => Option(t)
        case _ => none[T]
      })

    def parse(k: String, x: Any): ParsedValidated[Option[T]] =
      for {
        m <- Op.typeValue(parseMap(x).getOrElse(Map()))
        vo <- Op.downField(m.get(k), k)
        t <- vo.fold(Op.typeValue(none[T]))(as(_))
      } yield t
  }
}

object parsedany extends CatchOptionInstance {
  lazy implicit val cpoany = new CanParse[Option[Any], Any] {
    def as(x: Any): ParsedValidated[Option[Any]] =
      Op.typeValue(x match {
        case o: Option[Any] => o
        case y => Option(y)
      })

    def parse(k: String, x: Any): ParsedValidated[Option[Any]] =
      for {
        m <- Op.typeValue(parseMap(x).getOrElse(Map()))
        vo <- Op.downField(m.get(k), k)
        r <- vo.fold(Op.typeValue(none[Any]))(as(_))
      } yield r
  }

  import scalaz.syntax.nel._
  import scalaz.syntax.validation._
  import shapeless.HNil
  import shapeless.syntax.singleton._

  case class Fail(reason: String, ops: Vector[Op], failedOp: Op)

  val tp = new Typify[Fail, Any]

  implicit val parse2Error = (_: Parsed[Any], pe: ParseError) => Fail(pe.error, pe.ops, pe.failedOp)

  val checkEmail = Typify.validate((s: String) => typify.Validated(ops =>
    (ops, s).successNel[Fail].ensure(Fail("Email is invalid", ops, Op.ArrayIndex(1)).wrapNel)(_._2.contains("@"))))

  val checkAge = Typify.validate((i: Int) => typify.Validated(ops =>
    (ops, i).successNel[Fail].ensure(Fail("Too young", ops, Op.ArrayIndex(2)).wrapNel)(_._2 > 21)))

  val checkPerson = 'email ->> checkEmail :: 'age ->> checkAge :: HNil

  val passes: Any = Map("thing" -> Map("other" -> Map[Any, Any]("email" -> "foo@bar", "age" -> 22, "session" -> 77777, 3 -> "junk")))
  val fails: Any = Map("thing" -> Map("other" -> Map[Any, Any]("email" -> "fake email", "age" -> "foo", "session" -> 77777, 3 -> "junk")))

  import tp.syntax._

  val passed = Parsed(passes, Vector("thing", "other")).parse(checkPerson)
  val failed = Parsed(fails, Vector("thing", "other")).parse(checkPerson)
}
