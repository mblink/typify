package typify

import org.json4s.jackson.JsonMethods._
import org.json4s.JValue
import org.json4s.typify.parsedinstances._
import scalaz.Leibniz.===
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.{\/, NonEmptyList}
import shapeless.LabelledGeneric
import shapeless.tag
import shapeless.tag.@@

object Json4sExample extends App {

  trait Email {}
  trait Age {}
  trait SessId {}

  sealed trait Gender
  case object Male extends Gender
  case object Female extends Gender

  case class Person(email: String @@ Email, age: Int @@ Age, gender: Gender, session: Option[Int @@ SessId])
  case class UnsafePerson(email: String, age: Int)

  val typify = new Typify[String, JValue]
  import typify.parsers._

  implicit def e2l = (p: Parsed[JValue], e: ParseError) => s"${e.key}: ${e.error}"

  implicit lazy val vEmail = typify.validate[String, String @@ Email]((e: String) =>
    e.contains("@").option(tag[Email](e)).toSuccessNel("invalid email"))
  implicit lazy val vGender = typify.validate[String, Gender]((e: String) => e match {
    case "m" => Male.successNel[String]
    case "f" => Female.successNel[String]
    case x => s"Invalid gender $x".failureNel[Gender]
  })
  implicit lazy val vAge = typify.validate[Int, Int @@ Age]((a: Int) =>
    (a > 18).option(tag[Age](a)).toSuccessNel("too young"))
  implicit lazy val sid = typify.validate[Option[Int], Option[Int @@ SessId]]((i: Option[Int]) =>
    i match {
      case Some(id) => (id > 10000).option(Some(tag[SessId](id))).toSuccessNel(s"invalid session $id")
      case None => None.successNel[String]
    })

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


