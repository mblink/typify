package typify

import typify.parsedmap._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.ValidationNel
import shapeless.LabelledGeneric
import shapeless.tag
import shapeless.tag.@@

object Example extends App {

  trait Email {}
  trait Age {}
  trait SessId {}

  sealed trait Gender
  case object Male extends Gender
  case object Female extends Gender

  case class Person(email: String @@ Email, age: Long @@ Age, gender: Gender, session: Option[Int @@ SessId])
  case class UnsafePerson(email: String, age: Int)

  val typify = new Typify[String, Map[String, Any]]
  import typify.parsers._

  implicit lazy val e2s = (ps: Parsed[Map[String, Any]], p: ParseError) => s"${ps.root}, ${p.key}: ${p.error}"

  implicit lazy val vEmail = typify.validate[String, String @@ Email]((e: String) =>
    e.contains("@").option(tag[Email](e)).toSuccessNel("invalid email"))
  implicit lazy val vGender = typify.validate[String, Gender]((e: String) => e match {
    case "m" => Male.successNel[String]
    case "f" => Female.successNel[String]
    case x => s"Invalid gender $x".failureNel[Gender]
  })
  implicit lazy val vAge = typify.validate[Long, Long @@ Age]((k: String, a: Long, p: Parsed[Map[String, Any]]) =>
    (a > 18).option(tag[Age](a)).toSuccessNel(s"$k too young"))
  implicit lazy val sid = typify.validate[Option[Int], Option[Int @@ SessId]]((i: Option[Int]) =>
    i match {
      case Some(id) => (id > 10000).option(Some(tag[SessId](id))).toSuccessNel(s"invalid session $id")
      case None => None.successNel[String]
    })

  val bp = typify[Person](Map("emmail" -> "foo", "age" -> 17L, "gender" -> "ms", "session" -> Some(33)))
  println(bp)
  val p = typify[Person](Map("email" -> "foo", "age" -> 17L, "gender" -> "ms", "session" -> Some(33)))
  println(p)
  val pp = typify[(String @@ Email, Gender) => Person](Map("age" -> 23L, "session" -> None))
            .map(_(tag[Email]("boo@far"), Male))
  println(pp)
  // will not compile - println(typify[UnsafePerson](Map()))
}

