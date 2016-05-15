package typify

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.typify.parsedinstances._
import scalaz.std.option._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.ValidationNel
import shapeless.{::, HNil}
import shapeless.syntax.singleton._
import shapeless.record._
import typify.convert._
import typify.parsedmap._

@JSExport
object jsDynamicExample {

  val typify = new Typify[String, Parsed[js.Dynamic]]
  import typify.syntax._

  implicit lazy val e2s = (pd: Parsed[js.Dynamic], p: ParseError) => s"${p.key}: ${p.error}"

  val setup = new TestSetup(typify)
  import setup._

  @JSExport
  def optionalPerson(jsd: String): ValidationNel[String, Option[Person]] =
    Parsed(js.JSON.parse(jsd)).parseOption(person).map(_.map(_.convertTo[Person]))

  @JSExport
  def validatePerson(jsd: String): ValidationNel[String, Person] = {
    Parsed(js.JSON.parse(jsd)).parse(person).map(_.convertTo[Person])
  }

  case class Optional[A](a: Option[A])
  val pp = (p: js.Dynamic) => Parsed(p).parse(person).map(_.convertTo[Person])
  val opp = ('a ->> Typify.optional(pp)) :: HNil

  @JSExport
  def opPerson(jsd: String): ValidationNel[String, Optional[Person]] =
    Parsed(js.JSON.parse(jsd), Seq("b")).parse(opp).map(_.convertTo[Optional[Person]])

  @JSExport
  def partialValidatePerson(jsd: String, root: Seq[String] = Seq()) =
    Parsed(js.JSON.parse(jsd), root).parse(person - 'email - 'age)
}


