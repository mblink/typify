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
// tp: Typify[Fail, Any] = typify.Typify@37c7b6b6
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
// checkEmail: typify.package.KPV[Any, Fail, String] = typify.Typify$$$Lambda$24045/0x00000008038d0000@5137b660

val checkAge = Typify.validate((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Too young", c.history)))(_ > 21))
// checkAge: typify.package.KPV[Any, Fail, Int] = typify.Typify$$$Lambda$24045/0x00000008038d0000@6d67f97

val checkSessIdF = ((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Invalid session id", c.history)))(_ > 3000))
// checkSessIdF: (String, Int, Cursor[Any]) => cats.data.Validated[NonEmptyList[Fail], Int] = <function3>

val checkSessId = Typify.optional(checkSessIdF)
// checkSessId: typify.package.KPV[Any, Fail, Option[Int]] = typify.Typify$$$Lambda$24046/0x00000008038d03d0@32befbf6
```

Now we can define in which fields to look for these values under our source value as follows.

```scala
val checkPerson = "email" ->> checkEmail :: "age" ->> checkAge :: "session" ->> checkSessId :: HNil
// checkPerson: shapeless.::[typify.package.KPV[Any, Fail, String] with shapeless.labelled.KeyTag["email", typify.package.KPV[Any, Fail, String]], shapeless.::[typify.package.KPV[Any, Fail, Int] with shapeless.labelled.KeyTag["age", typify.package.KPV[Any, Fail, Int]], shapeless.::[typify.package.KPV[Any, Fail, Option[Int]] with shapeless.labelled.KeyTag["session", typify.package.KPV[Any, Fail, Option[Int]]], HNil]]] = typify.Typify$$$Lambda$24045/0x00000008038d0000@5137b660 :: typify.Typify$$$Lambda$24045/0x00000008038d0000@6d67f97 :: typify.Typify$$$Lambda$24046/0x00000008038d03d0@32befbf6 :: HNil
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
// passed: cats.data.package.ValidatedNel[Fail, typify.tuple.package.*:[typify.record.package.->>["email", String], typify.tuple.package.*:[typify.record.package.->>["age", Int], typify.tuple.package.*:[typify.record.package.->>["session", Option[Int]], typify.tuple.package.EmptyTuple]]]] = Valid(
//   a = "foo@bar" :: 22 :: None :: HNil
// )
val passedNoSess = Cursor.top(passesNoSess).parse(checkPerson)
// passedNoSess: cats.data.package.ValidatedNel[Fail, typify.tuple.package.*:[typify.record.package.->>["email", String], typify.tuple.package.*:[typify.record.package.->>["age", Int], typify.tuple.package.*:[typify.record.package.->>["session", Option[Int]], typify.tuple.package.EmptyTuple]]]] = Valid(
//   a = "foo@bar" :: 22 :: None :: HNil
// )
val failedAtParse = Cursor.top(failsAtParse).parse(checkPerson)
// failedAtParse: cats.data.package.ValidatedNel[Fail, typify.tuple.package.*:[typify.record.package.->>["email", String], typify.tuple.package.*:[typify.record.package.->>["age", Int], typify.tuple.package.*:[typify.record.package.->>["session", Option[Int]], typify.tuple.package.EmptyTuple]]]] = Invalid(
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
// failedAtValidation: cats.data.package.ValidatedNel[Fail, typify.tuple.package.*:[typify.record.package.->>["email", String], typify.tuple.package.*:[typify.record.package.->>["age", Int], typify.tuple.package.*:[typify.record.package.->>["session", Option[Int]], typify.tuple.package.EmptyTuple]]]] = Invalid(
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
import shapeless.record._

val checkRequiredSess = Typify.validate(checkSessIdF)
// checkRequiredSess: typify.package.KPV[Any, Fail, Int] = typify.Typify$$$Lambda$24045/0x00000008038d0000@729b81fe
val checkPersonWithSession = (checkPerson - "session") + ("session" ->> checkRequiredSess)
// checkPersonWithSession: shapeless.::[String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], String] with shapeless.labelled.KeyTag["email", String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], String]], shapeless.::[String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], Int] with shapeless.labelled.KeyTag["age", String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], Int]], shapeless.::[String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], Int] with shapeless.labelled.KeyTag["session", String => Cursor[Any] => cats.data.Validated[NonEmptyList[Fail], Int]], HNil]]] = typify.Typify$$$Lambda$24045/0x00000008038d0000@5137b660 :: typify.Typify$$$Lambda$24045/0x00000008038d0000@6d67f97 :: typify.Typify$$$Lambda$24045/0x00000008038d0000@729b81fe :: HNil

val passedWithSession = Cursor.top(passes).parse(checkPersonWithSession)
// passedWithSession: cats.data.package.ValidatedNel[Fail, typify.tuple.package.*:[typify.record.package.->>["email", String], typify.tuple.package.*:[typify.record.package.->>["age", Int], typify.tuple.package.*:[typify.record.package.->>["session", Int], typify.tuple.package.EmptyTuple]]]] = Valid(
//   a = "foo@bar" :: 22 :: 77777 :: HNil
// )
val failedNoSession = Cursor.top(passesNoSess).parse(checkPersonWithSession)
// failedNoSession: cats.data.package.ValidatedNel[Fail, typify.tuple.package.*:[typify.record.package.->>["email", String], typify.tuple.package.*:[typify.record.package.->>["age", Int], typify.tuple.package.*:[typify.record.package.->>["session", Int], typify.tuple.package.EmptyTuple]]]] = Invalid(
//   e = NonEmptyList(
//     head = Fail(
//       reason = "Could not be interpreted as Int",
//       history = CursorHistory(Vector(MoveTop), Some(NonEmptyVector(DownField(session))))
//     ),
//     tail = List()
//   )
// )
val passedPartialSession = Cursor.top(passesNoSess).parse(checkPersonWithSession - "session")
// passedPartialSession: cats.data.package.ValidatedNel[Fail, typify.tuple.package.*:[typify.record.package.->>["email", String], typify.tuple.package.*:[typify.record.package.->>["age", Int], typify.tuple.package.EmptyTuple]]] = Valid(
//   a = "foo@bar" :: 22 :: HNil
// )
```
