package typify

import play.api.libs.json.Json._
import play.api.libs.json.JsValue
import play.api.libs.json.typify.parsedinstances._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.{\/, NonEmptyList}
import shapeless.ops.hlist.LeftFolder
import shapeless.syntax.singleton._
import shapeless.record._
import shapeless.{::, HList, HNil}
import typify.convert._
import typify.convert.syntax._

object PlayJsonExample extends App {

  val typify = new Typify[String, Parsed[JsValue]]
  import typify.syntax._


  implicit def e2l = (p: Parsed[JsValue], e: ParseError) => s"${p.root}:${e.key}: ${e.error}"

  val setup = new TestSetup(typify)
  import setup._

  case class Optional[A](a: Option[A])
  case class Mandatory[A](a: A)
  val pp = (p: JsValue) => Parsed(p).parse(person).map(_.convertTo[Person])
  val opp = 'a ->> Typify.optional(pp) :: HNil
  val mpp = 'a ->> Typify.validate(pp) :: HNil

  val valid = Parsed(parse("""{"a":{"email":"foo@opman","age":"22","gender":"m","session":"77777"}}"""))

  println(valid.parse(opp).map(_.convertTo[Optional[Person]]))
  println(Parsed(parse("null")).parse(opp).map(_.convertTo[Optional[Person]]))
  println(Parsed(parse("""{"b":{"a":{"email":"foo@bar"}}}"""), Seq("b"))
            .parse(opp).map(_.convertTo[Optional[Person]]))
  println(valid.parse(mpp).map(_.convertTo[Mandatory[Person]]))
  println(Parsed(parse("null")).parse(mpp).map(_.convertTo[Mandatory[Person]]))

  val npps = Parsed(parse("null")).parseOption(person).map(_.map(_.convertTo[Person]))
  println(npps)
  val osps = Parsed(parse("""{"a":{"email":"foo@bar","age":22,"gender":"m","session":77777}}"""),
                Seq("b")).parseOption(person).map(_.map(_.convertTo[Person]))
  println(osps)
  val ofpf = Parsed(parse("""{"email":"foobar","age":2,"gender":"m","session":77777}"""))
                .parseOption(person).map(_.map(_.convertTo[Person]))
  println(ofpf)
  val opps = Parsed(parse("""{"email":"foobar","age":22,"gender":"m","session":77777}"""))
                .parseOption(person - 'email)
                .map(_.map(x => (x + ('email ->> "foo@bar")).convertTo[Person]))
  println(opps)
  val nppps = Parsed(parse("null"))
                .parseOption(person - 'email)
                .map(_.map(x => (x + ('email ->> "foobar")).convertTo[Person]))
  println(nppps)
  val p = Parsed(parse("""{"a":{"b":{"email":"foo","age":17,"gender":"ms","session":3}}}"""),
                Seq("a", "b"))
            .parse(person).map(_.convertTo[Person])
  println(p)
  val bpp = Parsed(parse("""{"foo":{"age":23}}"""), Seq("foo"))
            .parse(person - 'email - 'gender)
            .map(x => (x + ('email ->> "e") + ('gender ->> (Male: Gender))).convertTo[Person])
  println(bpp)
  println(Parsed(parse("[]"), Seq("a")).parse(person).map(_.convertTo[Person]))

  def parsed[R <: HList, G <: HList, P](i: R)(jv: Parsed[JsValue])(implicit
    lf: LeftFolder.Aux[R,typify.PV[HNil],typify.foldPV.type,typify.PV[G]]) =
    jv.parse(i)

  println(parsed(mpp)(valid).map(_.convertTo[Mandatory[Person]]))
}
