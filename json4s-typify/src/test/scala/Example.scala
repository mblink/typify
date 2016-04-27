package typify

import org.json4s.jackson.JsonMethods._
import org.json4s.JValue
import org.json4s.typify.parsedinstances._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._
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

  implicit lazy val genP = LabelledGeneric[Person]
  implicit lazy val genUP = LabelledGeneric[UnsafePerson]

  val typify = new Typify[String, JValue]
  import typify.parsers._

  implicit lazy val sp = typify.parseBasic[String](p => s"${p.key}: ${p.error}")
  implicit lazy val ip = typify.parseBasic[Int](p => s"${p.key} cannot be parsed as int")
  implicit lazy val osp = typify.parseBasic[Option[Int]](p => s"${p.key} cannot be parsed as Option[Int]")

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

  val p = typify[Person](parse("""{"email":"foo","age":17,"gender":"ms","session":3}"""))
  println(p)
  val pp = typify[(String @@ Email, Gender) => Person](parse("""{"foo":{"age":23}}"""), Seq("foo"))
  println(pp.map(_(tag[Email]("boo@far"), Male)))
  println(typify[Person](parse("[]")))
  // will not compile - println(typify[UnsafePerson](parse("{}")))
}


