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
import formless.tuple._
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
// tp: Typify[Fail, Any] = typify.Typify@18b3ee39
```

We also need to define an implicit function to convert a typify.ParseError to our failure type.

ParseError looks like this

```scala
case class ParseError(key: String, error: String)
```

```scala
implicit val parse2Error: ParseError[Any] => Fail = pe => Fail(pe.message, pe.cursor.history)
// parse2Error: Function1[ParseError[Any], Fail] = repl.MdocSession$MdocApp$$Lambda$13197/0x0000008003065000@2bbb6404
```

Now we can define some validation functions.
Let's validate an email, an age and an optional session id.

```scala
import cats.data.NonEmptyList
import cats.syntax.validated._

val checkEmail = Typify.validate((_: String, s: String, c: Cursor[Any]) => s.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Email is invalid", c.history)))(_.contains("@")))
// checkEmail: Function1[String, PV[Any, Fail, String]] = typify.Typify$$$Lambda$13199/0x0000008003069440@23bb1a57

val checkAge = Typify.validate((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Too young", c.history)))(_ > 21))
// checkAge: Function1[String, PV[Any, Fail, Int]] = typify.Typify$$$Lambda$13199/0x0000008003069440@43e05451

val checkSessIdF = ((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Invalid session id", c.history)))(_ > 3000))
// checkSessIdF: Function3[String, Int, Cursor[Any], Validated[NonEmptyList[Fail], Int]] = repl.MdocSession$MdocApp$$Lambda$13201/0x000000800306c690@595d283

val checkSessId = Typify.optional(checkSessIdF)
// checkSessId: Function1[String, PV[Any, Fail, Option[Int]]] = typify.Typify$$$Lambda$13202/0x0000008003069a40@757e0d71
```

Now we can define in which fields to look for these values under our source value as follows.

```scala
val checkPerson = ("email" ->> checkEmail) *: ("age" ->> checkAge) *: ("session" ->> checkSessId) *: EmptyTuple
// checkPerson: *:[->>["email", KPV[Any, Fail, String]], *:[->>["age", KPV[Any, Fail, Int]], *:[->>["session", KPV[Any, Fail, Option[Int]]], EmptyTuple]]] = (
//   typify.Typify$$$Lambda$13199/0x0000008003069440@23bb1a57,
//   typify.Typify$$$Lambda$13199/0x0000008003069440@43e05451,
//   typify.Typify$$$Lambda$13202/0x0000008003069a40@757e0d71
// )
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
// passed: Validated[NonEmptyList[Fail], *:[->>["email", String], *:[->>["age", Int], *:[->>["session", Option[Int]], EmptyTuple]]]] = Valid(
//   a = ("foo@bar", 22, None)
// )
val passedNoSess = Cursor.top(passesNoSess).parse(checkPerson)
// passedNoSess: Validated[NonEmptyList[Fail], *:[->>["email", String], *:[->>["age", Int], *:[->>["session", Option[Int]], EmptyTuple]]]] = Valid(
//   a = ("foo@bar", 22, None)
// )
val failedAtParse = Cursor.top(failsAtParse).parse(checkPerson)
// failedAtParse: Validated[NonEmptyList[Fail], *:[->>["email", String], *:[->>["age", Int], *:[->>["session", Option[Int]], EmptyTuple]]]] = Invalid(
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
// failedAtValidation: Validated[NonEmptyList[Fail], *:[->>["email", String], *:[->>["age", Int], *:[->>["session", Option[Int]], EmptyTuple]]]] = Invalid(
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
// checkRequiredSess: Function1[String, PV[Any, Fail, Int]] = typify.Typify$$$Lambda$13199/0x0000008003069440@2ae0d8a2
val checkPersonWithSession = checkPerson.updateWith("session")(_ => checkRequiredSess)
// checkPersonWithSession: *:[->>["email", KPV[Any, Fail, String]], *:[->>["age", KPV[Any, Fail, Int]], *:[->>["session", Function1[String, PV[Any, Fail, Int]]], EmptyTuple]]] = (
//   typify.Typify$$$Lambda$13199/0x0000008003069440@23bb1a57,
//   typify.Typify$$$Lambda$13199/0x0000008003069440@43e05451,
//   typify.Typify$$$Lambda$13199/0x0000008003069440@2ae0d8a2
// )

val passedWithSession = Cursor.top(passes).parse(checkPersonWithSession)
// passedWithSession: Validated[NonEmptyList[Fail], *:[->>["email", String], *:[->>["age", Int], *:[->>["session", Int], EmptyTuple]]]] = Valid(
//   a = ("foo@bar", 22, 77777)
// )
val failedNoSession = Cursor.top(passesNoSess).parse(checkPersonWithSession)
// failedNoSession: Validated[NonEmptyList[Fail], *:[->>["email", String], *:[->>["age", Int], *:[->>["session", Int], EmptyTuple]]]] = Invalid(
//   e = NonEmptyList(
//     head = Fail(
//       reason = "Could not be interpreted as Int",
//       history = CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(session))))
//     ),
//     tail = List()
//   )
// )
val passedPartialSession = Cursor.top(passesNoSess).parse(checkPersonWithSession - "session")
// passedPartialSession: Validated[NonEmptyList[Fail], *:[->>["email", String], *:[->>["age", Int], EmptyTuple]]] = Valid(
//   a = ("foo@bar", 22)
// )
```
