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
import formless.hlist._
import formless.record._
import typify.{Cursor, CursorHistory, ParseError, Typify}
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
// tp: Typify[Fail, Any] = typify.Typify@2bd9cc3e
```

We also need to define an implicit function to convert a typify.ParseError to our failure type.

ParseError looks like this

```scala
case class ParseError(key: String, error: String)
```

```scala
implicit val parse2Error: ParseError[Any] => Fail = pe => Fail(pe.message, pe.cursor.history)
// parse2Error: Function1[ParseError[Any], Fail] = repl.MdocSession$MdocApp$$Lambda/0x000000b8024b94f8@a5b553a
```

Now we can define some validation functions.
Let's validate an email, an age and an optional session id.

```scala
import cats.data.NonEmptyList
import cats.syntax.validated._

val checkEmail = Typify.validate((_: String, s: String, c: Cursor[Any]) => s.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Email is invalid", c.history)))(_.contains("@")))
// checkEmail: Function1[String, PV[Any, Fail, String]] = typify.Typify$$$Lambda/0x000000b8024c21b0@5924ffa8

val checkAge = Typify.validate((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Too young", c.history)))(_ > 21))
// checkAge: Function1[String, PV[Any, Fail, Int]] = typify.Typify$$$Lambda/0x000000b8024c21b0@1ee9de3b

val checkSessIdF = ((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Invalid session id", c.history)))(_ > 3000))
// checkSessIdF: Function3[String, Int, Cursor[Any], Validated[NonEmptyList[Fail], Int]] = repl.MdocSession$MdocApp$$Lambda/0x000000b8024c4460@3d77bc3f

val checkSessId = Typify.optional(checkSessIdF)
// checkSessId: Function1[String, PV[Any, Fail, Option[Int]]] = typify.Typify$$$Lambda/0x000000b8024c27a0@6d55cc2e
```

Now we can define in which fields to look for these values under our source value as follows.

```scala
val checkPerson = ("email" ->> checkEmail) :: ("age" ->> checkAge) :: ("session" ->> checkSessId) :: HNil
// checkPerson: ::[->>["email", KPV[Any, Fail, String]], ::[->>["age", KPV[Any, Fail, Int]], ::[->>["session", KPV[Any, Fail, Option[Int]]], HNil]]] = typify.Typify$$$Lambda/0x000000b8024c21b0@5924ffa8 :: typify.Typify$$$Lambda/0x000000b8024c21b0@1ee9de3b :: typify.Typify$$$Lambda/0x000000b8024c27a0@6d55cc2e :: HNil
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
// passed: Validated[NonEmptyList[Fail], ::[->>["email", String], ::[->>["age", Int], ::[->>["session", Option[Int]], HNil]]]] = Valid(
//   a = "foo@bar" :: 22 :: None :: HNil
// )
val passedNoSess = Cursor.top(passesNoSess).parse(checkPerson)
// passedNoSess: Validated[NonEmptyList[Fail], ::[->>["email", String], ::[->>["age", Int], ::[->>["session", Option[Int]], HNil]]]] = Valid(
//   a = "foo@bar" :: 22 :: None :: HNil
// )
val failedAtParse = Cursor.top(failsAtParse).parse(checkPerson)
// failedAtParse: Validated[NonEmptyList[Fail], ::[->>["email", String], ::[->>["age", Int], ::[->>["session", Option[Int]], HNil]]]] = Invalid(
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
// failedAtValidation: Validated[NonEmptyList[Fail], ::[->>["email", String], ::[->>["age", Int], ::[->>["session", Option[Int]], HNil]]]] = Invalid(
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

Because our validation rules and results are both simply HLists, we can use HList and record
operations to compose rules, and do partial validation.

```scala
val checkRequiredSess = Typify.validate(checkSessIdF)
// checkRequiredSess: Function1[String, PV[Any, Fail, Int]] = typify.Typify$$$Lambda/0x000000b8024c21b0@50d15db7
val checkPersonWithSession = checkPerson.updateWith("session")(_ => checkRequiredSess)
// checkPersonWithSession: ::[->>["email", KPV[Any, Fail, String]], ::[->>["age", KPV[Any, Fail, Int]], ::[->>["session", Function1[String, PV[Any, Fail, Int]]], HNil]]] = typify.Typify$$$Lambda/0x000000b8024c21b0@5924ffa8 :: typify.Typify$$$Lambda/0x000000b8024c21b0@1ee9de3b :: typify.Typify$$$Lambda/0x000000b8024c21b0@50d15db7 :: HNil

val passedWithSession = Cursor.top(passes).parse(checkPersonWithSession)
// passedWithSession: Validated[NonEmptyList[Fail], ::[->>["email", String], ::[->>["age", Int], ::[->>["session", Int], HNil]]]] = Valid(
//   a = "foo@bar" :: 22 :: 77777 :: HNil
// )
val failedNoSession = Cursor.top(passesNoSess).parse(checkPersonWithSession)
// failedNoSession: Validated[NonEmptyList[Fail], ::[->>["email", String], ::[->>["age", Int], ::[->>["session", Int], HNil]]]] = Invalid(
//   e = NonEmptyList(
//     head = Fail(
//       reason = "Could not be interpreted as Int",
//       history = CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(session))))
//     ),
//     tail = List()
//   )
// )
val passedPartialSession = Cursor.top(passesNoSess).parse(checkPersonWithSession - "session")
// passedPartialSession: Validated[NonEmptyList[Fail], ::[->>["email", String], ::[->>["age", Int], HNil]]] = Valid(
//   a = "foo@bar" :: 22 :: HNil
// )
```
