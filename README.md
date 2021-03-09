# Typify

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
import typify.parsedany._

case class Fail(reason: String, history: CursorHistory[_])

val tp = new Typify[Fail, Any]
// tp: Typify[Fail, Any] = typify.Typify@6dcf6919
```

We also need to define an implicit function to convert a typify.ParseError to our failure type.

ParseError looks like this

```scala
case class ParseError(key: String, error: String)
```

```scala
implicit val parse2Error = (pe: ParseError[Any]) => Fail(pe.message, pe.cursor.history)
// parse2Error: ParseError[Any] => Fail = <function1>
```

Now we can define some validation functions.
Let's validate an email, an age and an optional session id.

```scala
import cats.data.NonEmptyList
import cats.syntax.validated._

val checkEmail = Typify.validate((_: String, s: String, c: Cursor[Any]) => s.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Email is invalid", c.history)))(_.contains("@")))
// checkEmail: String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, String] = typify.Typify$$$Lambda$20077/1435301536@154988ed

val checkAge = Typify.validate((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Too young", c.history)))(_ > 21))
// checkAge: String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, Int] = typify.Typify$$$Lambda$20077/1435301536@63f7b345

val checkSessIdF = ((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Invalid session id", c.history)))(_ > 3000))
// checkSessIdF: (String, Int, Cursor[Any]) => cats.data.Validated[NonEmptyList[Fail], Int] = <function3>

val checkSessId = Typify.optional(checkSessIdF)
// checkSessId: String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, Option[Int]] = typify.Typify$$$Lambda$20079/1812630375@790d6077
```

Now we can define in which fields to look for these values under our source value as follows.

```scala
val checkPerson = 'email ->> checkEmail :: 'age ->> checkAge :: 'session ->> checkSessId :: HNil
// checkPerson: shapeless.::[String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, String] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[email], String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, String]], shapeless.::[String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[age], String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, Int]], shapeless.::[String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, Option[Int]] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[session], String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, Option[Int]]], HNil]]] = typify.Typify$$$Lambda$20077/1435301536@154988ed :: typify.Typify$$$Lambda$20077/1435301536@63f7b345 :: typify.Typify$$$Lambda$20079/1812630375@790d6077 :: HNil
```

From here we are able to parse a person out of Any using our Typify instance.

```scala
import tp.syntax._

val passes: Any = Map("email" -> "foo@bar", "age" -> 22, "session" -> 77777, 3 -> "junk")
// passes: Any = Map(
//   "email" -> "foo@bar",
//   "age" -> 22,
//   "session" -> 77777,
//   3 -> "junk"
// )
val passesNoSess: Any = Map("email" -> "foo@bar", "age" -> 22, 500L -> "extra doesnt matter")
// passesNoSess: Any = Map(
//   "email" -> "foo@bar",
//   "age" -> 22,
//   500L -> "extra doesnt matter"
// )
val failsAtParse: Any = 33
// failsAtParse: Any = 33
val failsAtValidation: Any = Map("email" -> "foo", "session" -> 77777)
// failsAtValidation: Any = Map("email" -> "foo", "session" -> 77777)

val passed = Cursor.top(passes).parse(checkPerson)
// passed: cats.data.package.ValidatedNel[Fail, shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[email], String], shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[age], Int], shapeless.::[Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[session], Option[Int]], HNil]]]] = Valid(
//   a = "foo@bar" :: 22 :: None :: HNil
// )
val passedNoSess = Cursor.top(passesNoSess).parse(checkPerson)
// passedNoSess: cats.data.package.ValidatedNel[Fail, shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[email], String], shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[age], Int], shapeless.::[Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[session], Option[Int]], HNil]]]] = Valid(
//   a = "foo@bar" :: 22 :: None :: HNil
// )
val failedAtParse = Cursor.top(failsAtParse).parse(checkPerson)
// failedAtParse: cats.data.package.ValidatedNel[Fail, shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[email], String], shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[age], Int], shapeless.::[Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[session], Option[Int]], HNil]]]] = Invalid(
//   e = NonEmptyList(
//     head = Fail(
//       reason = "Could not be interpreted as java.lang.String",
//       history = CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(email))))
//     ),
//     tail = List(
//       Fail(
//         reason = "Could not be interpreted as Int",
//         history = CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(age))))
//       )
//     )
//   )
// )
val failedAtValidation = Cursor.top(failsAtValidation).parse(checkPerson)
// failedAtValidation: cats.data.package.ValidatedNel[Fail, shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[email], String], shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[age], Int], shapeless.::[Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[session], Option[Int]], HNil]]]] = Invalid(
//   e = NonEmptyList(
//     head = Fail(
//       reason = "Email is invalid",
//       history = CursorHistory(Vector(DownField(email), MoveTop), None)
//     ),
//     tail = List(
//       Fail(
//         reason = "Could not be interpreted as Int",
//         history = CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(age))))
//       )
//     )
//   )
// )
```

Note that a successful validation returns an HList. We can easily convert it to a compatible case
class. Field order is not important, and supersets of target types are allowed.

These conversions are type safe. Attempting to convert to a case class that requires fields which
are not present on a given HList will fail at compile time.

```scala
case class Person(age: Int, email: String)
case class PersonWithSession(session: Option[Int], email: String, age: Int)

passed.map(_.convertTo[Person])
// res0: cats.data.Validated[NonEmptyList[Fail], Person] = Valid(
//   a = Person(age = 22, email = "foo@bar")
// )
passed.map(_.convertTo[PersonWithSession])
// res1: cats.data.Validated[NonEmptyList[Fail], PersonWithSession] = Valid(
//   a = PersonWithSession(session = None, email = "foo@bar", age = 22)
// )
passedNoSess.map(_.convertTo[Person])
// res2: cats.data.Validated[NonEmptyList[Fail], Person] = Valid(
//   a = Person(age = 22, email = "foo@bar")
// )
passedNoSess.map(_.convertTo[PersonWithSession])
// res3: cats.data.Validated[NonEmptyList[Fail], PersonWithSession] = Valid(
//   a = PersonWithSession(session = None, email = "foo@bar", age = 22)
// )
```

Because our validation rules and results are both simply HLists, we can use HList and record
operations to compose rules, and do partial validation.

```scala
import shapeless.record._

val checkRequiredSess = Typify.validate(checkSessIdF)
// checkRequiredSess: String => Cursor[Any] => cats.data.package.ValidatedNel[Fail, Int] = typify.Typify$$$Lambda$20077/1435301536@42275571
val checkPersonWithSession = (checkPerson - 'session) + ('session ->> checkRequiredSess)
// checkPersonWithSession: shapeless.::[String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], String] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[email], String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], String]], shapeless.::[String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[age], String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], Int]], shapeless.::[String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[session], String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], Int]], HNil]]] = typify.Typify$$$Lambda$20077/1435301536@154988ed :: typify.Typify$$$Lambda$20077/1435301536@63f7b345 :: typify.Typify$$$Lambda$20077/1435301536@42275571 :: HNil

val passedWithSession = Cursor.top(passes).parse(checkPersonWithSession)
// passedWithSession: cats.data.package.ValidatedNel[Fail, shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[email], String], shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[age], Int], shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[session], Int], HNil]]]] = Valid(
//   a = "foo@bar" :: 22 :: 77777 :: HNil
// )
val failedNoSession = Cursor.top(passesNoSess).parse(checkPersonWithSession)
// failedNoSession: cats.data.package.ValidatedNel[Fail, shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[email], String], shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[age], Int], shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[session], Int], HNil]]]] = Invalid(
//   e = NonEmptyList(
//     head = Fail(
//       reason = "Could not be interpreted as Int",
//       history = CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(session))))
//     ),
//     tail = List()
//   )
// )
val passedPartialSession = Cursor.top(passesNoSess).parse(checkPersonWithSession - 'session)
// passedPartialSession: cats.data.package.ValidatedNel[Fail, shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[email], String], shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[age], Int], HNil]]] = Valid(
//   a = "foo@bar" :: 22 :: HNil
// )

case class PersonRequireSession(session: Int, email: String, age: Int)

passedWithSession.map(_.convertTo[PersonRequireSession])
// res4: cats.data.Validated[NonEmptyList[Fail], PersonRequireSession] = Valid(
//   a = PersonRequireSession(session = 77777, email = "foo@bar", age = 22)
// )
failedNoSession.map(_.convertTo[PersonRequireSession])
// res5: cats.data.Validated[NonEmptyList[Fail], PersonRequireSession] = Invalid(
//   e = NonEmptyList(
//     head = Fail(
//       reason = "Could not be interpreted as Int",
//       history = CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(session))))
//     ),
//     tail = List()
//   )
// )
passedPartialSession.map(_ + ('session ->> 7777)).map(_.convertTo[PersonRequireSession])
// res6: cats.data.Validated[NonEmptyList[Fail], PersonRequireSession] = Valid(
//   a = PersonRequireSession(session = 7777, email = "foo@bar", age = 22)
// )
```
