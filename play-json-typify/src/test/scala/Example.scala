package typify

import play.api.libs.json.Json._
import play.api.libs.json.JsValue
import play.api.libs.json.typify.parsedinstances._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.{\/, NonEmptyList}
import shapeless.tag
import shapeless.tag.@@

object PlayJsonExample extends App {

  trait Email {}
  trait Age {}
  trait SessId {}

  sealed trait Gender
  case object Male extends Gender
  case object Female extends Gender

  case class Person(email: String @@ Email, age: Long @@ Age, gender: Gender, session: Option[Int @@ SessId])
  case class UnsafePerson(email: String, age: Int)

  val typify = new Typify[String, JsValue]
  import typify.parsers._

  implicit def e2l = (p: Parsed[JsValue], e: ParseError) => s"${p.root}:${e.key}: ${e.error}"

  implicit lazy val vEmail = typify.validate[String, String @@ Email]((e: String) =>
    e.contains("@").option(tag[Email](e)).toSuccessNel("invalid email"))
  implicit lazy val vGender = typify.validate[String, Gender]((e: String) => e match {
    case "m" => Male.successNel[String]
    case "f" => Female.successNel[String]
    case x => s"Invalid gender $x".failureNel[Gender]
  })
  implicit lazy val vAge = typify.validate[Long, Long @@ Age]((a: Long) =>
    (a > 18).option(tag[Age](a)).toSuccessNel("too young"))
  implicit lazy val sid = typify.validate[Option[Int], Option[Int @@ SessId]]((i: Option[Int]) =>
    i match {
      case Some(id) => (id > 10000).option(Some(tag[SessId](id))).toSuccessNel(s"invalid session $id")
      case None => None.successNel[String]
    })

  case class Optional[A](a: Option[A])
  case class Mandatory[A](a: A)
  val valid = parse("""{"a":{"email":"foo@opman","age":22,"gender":"m","session":77777}}""")

  println(typify[Optional[Person]](valid))
  println(typify[Optional[Person]](parse("null")))
  println(typify[Optional[Person]](parse("""{"b":{"a":{"email":"foo@bar"}}}"""), Seq("b")))
  println(typify[Mandatory[Person]](valid))
  println(typify[Mandatory[Person]](parse("null")))

  val npps = typify[Option[Person]](parse("null"))
  println(npps)
  val osps = typify[Option[Person]](parse("""{"email":"foo@bar","age":22,"gender":"m","session":77777}"""))
  println(osps)
  val ofpf = typify[Option[Person]](parse("""{"email":"foobar","age":2,"gender":"m","session":77777}"""))
  println(ofpf)
  val opps = typify[Option[String @@ Email => Person]](parse("""{"email":"foobar","age":22,"gender":"m","session":77777}"""))
  println(opps.map(_.map(_(tag[Email]("foo@partial")))))
  val nppps = typify[Option[String @@ Email => Person]](parse("null"))
  println(nppps.map(_.map(_(tag[Email]("foo@bar")))))
  val p = typify[Person](parse("""{"email":"foo","age":17,"gender":"ms","session":3}"""))
  println(p)
  val pp = typify[(String @@ Email, Gender) => Person](parse("""{"foo":{"age":23}}"""), Seq("foo"))
  println(pp.map(_(tag[Email]("boo@far"), Male)))
  println(typify[Person](parse("[]")))
  // will not compile - println(typify[UnsafePerson](parse("{}")))
}


