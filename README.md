#Typify

Typify is a library for parsing and validating poorly-typed data into well-typed data.

While it currently provides support for circe, play-json, and the Any type,
new source types can be added easily by implementing a type class for your source type.

Typify also does not prescribe a failure type, so you are able to accumulate parse/validation
failures of your own type with whatever data is relevant to your use case.

Validation rules and results are bare shapeless HLists, so shapeless record and HList
operations are available to compose and manipulate validations and results. This easily
supports re-use of validation rules, and partial parsing of inputs.

It is best understood through an example.

Let's say we want to validate a user, making sure that they have a valid email, age,
and an optional session id.

First some imports.

```scala
import shapeless.HNil
import shapeless.syntax.singleton._
import typify.{Cursor, CursorHistory, ParseError, Typify}
import typify.convert._
import typify.convert.syntax._
```

Now we can create an instance of Typify  by specifying the failure type we will use and
the type we will parse from.

Typify currently includes support for parsing from Any, circe, and play json.
New types can be added by implementing the CanParse typeclass for your desired source type.

Let's use Any for this example.

```scala
scala> import typify.parsedany._
import typify.parsedany._

scala> case class Fail(reason: String, history: CursorHistory[_])
defined class Fail

scala> val tp = new Typify[Fail, Any]
tp: typify.Typify[Fail,Any] = typify.Typify@2d95ba7a
```

We also need to define an implicit function to convert a typify.ParseError to our failure type.

ParseError looks like this

```scala
case class ParseError(key: String, error: String)
```

```scala
scala> implicit val parse2Error = (pe: ParseError[Any]) => Fail(pe.message, pe.cursor.history)
parse2Error: typify.ParseError[Any] => Fail = $$Lambda$25945/1069441974@563e4b11
```

Now we can define some validation functions.
Let's validate an email, an age and an optional session id.

```scala
scala> import cats.data.NonEmptyList
import cats.data.NonEmptyList

scala> import cats.syntax.validated._
import cats.syntax.validated._

scala> val checkEmail = Typify.validate((_: String, s: String, c: Cursor[Any]) => s.validNel[Fail]
     |   .ensure(NonEmptyList.of(Fail("Email is invalid", c.history)))(_.contains("@")))
checkEmail: typify.KPV[Any,Fail,String] = typify.Typify$$$Lambda$26043/1370013472@c2a3530

scala> val checkAge = Typify.validate((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
     |   .ensure(NonEmptyList.of(Fail("Too young", c.history)))(_ > 21))
checkAge: typify.KPV[Any,Fail,Int] = typify.Typify$$$Lambda$26043/1370013472@784aa045

scala> val checkSessIdF = ((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
     |   .ensure(NonEmptyList.of(Fail("Invalid session id", c.history)))(_ > 3000))
checkSessIdF: (String, Int, typify.Cursor[Any]) => cats.data.Validated[cats.data.NonEmptyList[Fail],Int] = $$Lambda$26052/697725526@53daefd

scala> val checkSessId = Typify.optional(checkSessIdF)
checkSessId: typify.KPV[Any,Fail,Option[Int]] = typify.Typify$$$Lambda$26053/1422044065@56fd9567
```

Now we can define in which fields to look for these values under our source value as follows.

```scala
scala> val checkPerson = 'email ->> checkEmail :: 'age ->> checkAge :: 'session ->> checkSessId :: HNil
checkPerson: typify.KPV[Any,Fail,String] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],typify.KPV[Any,Fail,String]] :: typify.KPV[Any,Fail,Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],typify.KPV[Any,Fail,Int]] :: typify.KPV[Any,Fail,Option[Int]] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],typify.KPV[Any,Fail,Option[Int]]] :: shapeless.HNil = typify.Typify$$$Lambda$26043/1370013472@c2a3530 :: typify.Typify$$$Lambda$26043/1370013472@784aa045 :: typify.Typify$$$Lambda$26053/1422044065@56fd9567 :: HNil
```

From here we are able to parse a person out of Any using our Typify instance.

```scala
scala> import tp.syntax._
import tp.syntax._

scala> val passes: Any = Map("email" -> "foo@bar", "age" -> 22, "session" -> 77777, 3 -> "junk")
passes: Any = Map(email -> foo@bar, age -> 22, session -> 77777, 3 -> junk)

scala> val passesNoSess: Any = Map("email" -> "foo@bar", "age" -> 22, 500L -> "extra doesnt matter")
passesNoSess: Any = Map(email -> foo@bar, age -> 22, 500 -> extra doesnt matter)

scala> val failsAtParse: Any = 33
failsAtParse: Any = 33

scala> val failsAtValidation: Any = Map("email" -> "foo", "session" -> 77777)
failsAtValidation: Any = Map(email -> foo, session -> 77777)

scala> val passed = Cursor.top(passes).parse(checkPerson)
passed: cats.data.ValidatedNel[Fail,String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String] :: Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int] :: Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Option[Int]] :: shapeless.HNil] = Valid(foo@bar :: 22 :: None :: HNil)

scala> val passedNoSess = Cursor.top(passesNoSess).parse(checkPerson)
passedNoSess: cats.data.ValidatedNel[Fail,String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String] :: Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int] :: Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Option[Int]] :: shapeless.HNil] = Valid(foo@bar :: 22 :: None :: HNil)

scala> val failedAtParse = Cursor.top(failsAtParse).parse(checkPerson)
failedAtParse: cats.data.ValidatedNel[Fail,String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String] :: Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int] :: Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Option[Int]] :: shapeless.HNil] = Invalid(NonEmptyList(Fail(Could not be interpreted as java.lang.String,CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(email))))), Fail(Could not be interpreted as Int,CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(age)))))))

scala> val failedAtValidation = Cursor.top(failsAtValidation).parse(checkPerson)
failedAtValidation: cats.data.ValidatedNel[Fail,String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String] :: Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int] :: Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Option[Int]] :: shapeless.HNil] = Invalid(NonEmptyList(Fail(Email is invalid,CursorHistory(Vector(DownField(email), MoveTop), None)), Fail(Could not be interpreted as Int,CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(age)))))))
```

Note that a successful validation returns an HList. We can easily convert it to a compatible case
class. Field order is not important, and supersets of target types are allowed.

These conversions are type safe. Attempting to convert to a case class that requires fields which
are not present on a given HList will fail at compile time.

```scala
scala> case class Person(age: Int, email: String)
defined class Person

scala> case class PersonWithSession(session: Option[Int], email: String, age: Int)
defined class PersonWithSession

scala> passed.map(_.convertTo[Person])
res0: cats.data.Validated[cats.data.NonEmptyList[Fail],Person] = Valid(Person(22,foo@bar))

scala> passed.map(_.convertTo[PersonWithSession])
res1: cats.data.Validated[cats.data.NonEmptyList[Fail],PersonWithSession] = Valid(PersonWithSession(None,foo@bar,22))

scala> passedNoSess.map(_.convertTo[Person])
res2: cats.data.Validated[cats.data.NonEmptyList[Fail],Person] = Valid(Person(22,foo@bar))

scala> passedNoSess.map(_.convertTo[PersonWithSession])
res3: cats.data.Validated[cats.data.NonEmptyList[Fail],PersonWithSession] = Valid(PersonWithSession(None,foo@bar,22))
```

Because our validation rules and results are both simply HLists, we can use HList and record
operations to compose rules, and do partial validation.

```scala
scala> import shapeless.record._
import shapeless.record._

scala> val checkRequiredSess = Typify.validate(checkSessIdF)
checkRequiredSess: typify.KPV[Any,Fail,Int] = typify.Typify$$$Lambda$26043/1370013472@306605b5

scala> val checkPersonWithSession = (checkPerson - 'session) + ('session ->> checkRequiredSess)
checkPersonWithSession: String => (typify.Cursor[Any] => cats.data.Validated[cats.data.NonEmptyList[Fail],String]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String => (typify.Cursor[Any] => cats.data.Validated[cats.data.NonEmptyList[Fail],String])] :: String => (typify.Cursor[Any] => cats.data.Validated[cats.data.NonEmptyList[Fail],Int]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],String => (typify.Cursor[Any] => cats.data.Validated[cats.data.NonEmptyList[Fail],Int])] :: String => (typify.Cursor[Any] => cats.data.Validated[cats.data.NonEmptyList[Fail],Int]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],String => (typify.Cursor[Any] => cats.data.Validated[c...

scala> val passed = Cursor.top(passes).parse(checkPersonWithSession)
passed: cats.data.ValidatedNel[Fail,String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String] :: Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int] :: Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Int] :: shapeless.HNil] = Valid(foo@bar :: 22 :: 77777 :: HNil)

scala> val failedNoSession = Cursor.top(passesNoSess).parse(checkPersonWithSession)
failedNoSession: cats.data.ValidatedNel[Fail,String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String] :: Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int] :: Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Int] :: shapeless.HNil] = Invalid(NonEmptyList(Fail(Could not be interpreted as Int,CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(session)))))))

scala> val passedPartialSession = Cursor.top(passesNoSess).parse(checkPersonWithSession - 'session)
passedPartialSession: cats.data.ValidatedNel[Fail,String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String] :: Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int] :: shapeless.HNil] = Valid(foo@bar :: 22 :: HNil)

scala> case class PersonRequireSession(session: Int, email: String, age: Int)
defined class PersonRequireSession

scala> passed.map(_.convertTo[PersonRequireSession])
res4: cats.data.Validated[cats.data.NonEmptyList[Fail],PersonRequireSession] = Valid(PersonRequireSession(77777,foo@bar,22))

scala> failedNoSession.map(_.convertTo[PersonRequireSession])
res5: cats.data.Validated[cats.data.NonEmptyList[Fail],PersonRequireSession] = Invalid(NonEmptyList(Fail(Could not be interpreted as Int,CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(session)))))))

scala> passedPartialSession.map(_ + ('session ->> 7777)).map(_.convertTo[PersonRequireSession])
res6: cats.data.Validated[cats.data.NonEmptyList[Fail],PersonRequireSession] = Valid(PersonRequireSession(7777,foo@bar,22))
```
