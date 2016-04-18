package typify

import parsers._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import shapeless.LabelledGeneric
import shapeless.tag
import shapeless.tag.@@

object Example extends App {

  implicit lazy val sp = stringParser[String, Map[String, Any]](p => s"${p.key}: ${p.error}")
  implicit lazy val ip = intParser[String, Map[String, Any]](p => s"${p.key} cannot be parsed as int")

  trait Email {}
  trait Age {}

  case class Person(email: String @@ Email, age: Int @@ Age)
  case class UnsafePerson(email: String, age: Int)

  implicit lazy val genP = LabelledGeneric[Person]
  implicit lazy val genUP = LabelledGeneric[UnsafePerson]

  implicit lazy val vEmail = Typify.validate[String, Map[String, Any], String, String @@ Email]((e: String) =>
    e.contains("@").option(tag[Email](e)).toSuccessNel("invalid email"))
  implicit lazy val vAge = Typify.validate[String, Map[String, Any], Int, Int @@ Age](a =>
    (a > 18).option(tag[Age](a)).toSuccessNel("too young"))

  val p = Typify[String, Map[String, Any], Person](Map("email" -> "foo", "age" -> 17))
  println(p)
  // will not compile - println(Typify[String, Map[String, Any], UnsafePerson](Map()))
}

