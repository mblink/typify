package typify

import typify.convert._
import typify.parsedmap._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.ValidationNel
import shapeless.{::, HNil}
import shapeless.syntax.singleton._
import shapeless.record._

class TestSetup[P](tp: Typify[String, Parsed[P]])(implicit
    e2l: Typify.E2L[String, P], cps: CanParse[String, P],
    cpoi: CanParse[Option[Int], P], cpi: CanParse[Int, P],
    cpp: CanParse[P, P], cpop: CanParse[Option[P], P],
    cpl: CanParse[Long, P]) {
  sealed trait Gender
  case object Male extends Gender
  case object Female extends Gender

  case class Person(email: String, age: Long, gender: Gender, session: Option[Int])

  val email = Typify.validate((e: String) =>
    e.contains("@").option(e).toSuccessNel("invalid email"))
  val gender = Typify.validate((e: String) => e match {
    case "m" => Male.successNel[String]
    case "f" => Female.successNel[String]
    case x => s"Invalid gender $x".failureNel[Gender]
  })
  val age = Typify.validate((k: String, a: Long, p: Parsed[P]) =>
    (a > 18).option(a).toSuccessNel(s"$k too young"))

  val sid = Typify.optional((id: Int) =>
    (id > 10000).option(id).toSuccessNel(s"invalid session $id"))

  val person = 'email ->> email :: 'gender ->> gender ::
               'age ->> age :: 'session ->> sid :: HNil

}

object Example extends App {

  val typify = new Typify[String, Parsed[Map[String, Any]]]
  import typify.syntax._

  implicit lazy val e2s = (ps: Parsed[Map[String, Any]], p: ParseError) => s"${ps.root}, ${p.key}: ${p.error}"

  val setup = new TestSetup(typify)
  import setup._

  val bp = Parsed(Map("emmail" -> "foo", "age" -> 17L, "gender" -> "ms", "session" -> Some(33)))
            .parse(person).map(_.convertTo[Person])
  println(bp)

  val p = Parsed(Map("email" -> "foo", "age" -> 17L, "gender" -> "ms", "session" -> Some(33)))
            .parse(person).map(_.convertTo[Person])
  println(p)
  val pp = Parsed(Map("age" -> 23L, "session" -> None))
             .parse(person - 'email - 'gender).map(x =>
                (x + ('email ->> "foo@bar") + ('gender ->> (Male: Gender))).convertTo[Person])
  println(pp)
}

