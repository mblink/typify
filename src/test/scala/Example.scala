package typify

import typify.parsedinstances._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import shapeless.LabelledGeneric
import shapeless.tag
import shapeless.tag.@@

object Example extends App {

  trait Email {}
  trait Age {}

  case class Person(email: String @@ Email, age: Int @@ Age)
  case class UnsafePerson(email: String, age: Int)

  implicit lazy val genP = LabelledGeneric[Person]
  implicit lazy val genUP = LabelledGeneric[UnsafePerson]

  val typify = new Typify[String, Map[String, Any]]
  import typify.parsers._

  implicit lazy val sp = typify.stringParser(p => s"${p.key}: ${p.error}")
  implicit lazy val ip = typify.intParser(p => s"${p.key} cannot be parsed as int")


  implicit lazy val vEmail = typify.validate[String, String @@ Email]((e: String) =>
    e.contains("@").option(tag[Email](e)).toSuccessNel("invalid email"))
  implicit lazy val vAge = typify.validate[Int, Int @@ Age](a =>
    (a > 18).option(tag[Age](a)).toSuccessNel("too young"))

  val p = typify[Person](Map("email" -> "foo", "age" -> 17))
  println(p)
  val pp = typify[String @@ Email => Person](Map("age" -> 23))
  println(pp.map(_(tag[Email]("boo@far"))))
  // will not compile - println(typify[UnsafePerson](Map()))
}

