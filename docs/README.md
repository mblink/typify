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

```scala mdoc:silent
import shapeless.HNil
import shapeless.syntax.singleton._
import typify.{Cursor, CursorHistory, ParseError, Typify}
```

Now we can create an instance of Typify  by specifying the failure type we will use and
the type we will parse from.

Typify currently includes support for parsing from Any, circe, and play json.
New types can be added by implementing the CanParse typeclass for your desired source type.

Let's use Any for this example.

```scala mdoc
import typify.parsedany._

case class Fail(reason: String, history: CursorHistory[_])

val tp = new Typify[Fail, Any]
```

We also need to define an implicit function to convert a typify.ParseError to our failure type.

ParseError looks like this

```scala
case class ParseError(key: String, error: String)
```

```scala mdoc
implicit val parse2Error = (pe: ParseError[Any]) => Fail(pe.message, pe.cursor.history)
```

Now we can define some validation functions.
Let's validate an email, an age and an optional session id.

```scala mdoc
import cats.data.NonEmptyList
import cats.syntax.validated._

val checkEmail = Typify.validate((_: String, s: String, c: Cursor[Any]) => s.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Email is invalid", c.history)))(_.contains("@")))

val checkAge = Typify.validate((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Too young", c.history)))(_ > 21))

val checkSessIdF = ((_: String, i: Int, c: Cursor[Any]) => i.validNel[Fail]
  .ensure(NonEmptyList.of(Fail("Invalid session id", c.history)))(_ > 3000))

val checkSessId = Typify.optional(checkSessIdF)
```

Now we can define in which fields to look for these values under our source value as follows.

```scala mdoc
val checkPerson = "email" ->> checkEmail :: "age" ->> checkAge :: "session" ->> checkSessId :: HNil
```

From here we are able to parse a person out of Any using our Typify instance.

```scala mdoc
import tp.syntax._

val passes: Any = Map("email" -> "foo@bar", "age" -> 22, "session" -> 77777, 3 -> "junk")
val passesNoSess: Any = Map("email" -> "foo@bar", "age" -> 22, 500L -> "extra doesnt matter")
val failsAtParse: Any = 33
val failsAtValidation: Any = Map("email" -> "foo", "session" -> 77777)

val passed = Cursor.top(passes).parse(checkPerson)
val passedNoSess = Cursor.top(passesNoSess).parse(checkPerson)
val failedAtParse = Cursor.top(failsAtParse).parse(checkPerson)
val failedAtValidation = Cursor.top(failsAtValidation).parse(checkPerson)
```

Because our validation rules and results are both simply HLists, we can use HList and record
operations to compose rules, and do partial validation.

```scala mdoc
import shapeless.record._

val checkRequiredSess = Typify.validate(checkSessIdF)
val checkPersonWithSession = (checkPerson - "session") + ("session" ->> checkRequiredSess)

val passedWithSession = Cursor.top(passes).parse(checkPersonWithSession)
val failedNoSession = Cursor.top(passesNoSess).parse(checkPersonWithSession)
val passedPartialSession = Cursor.top(passesNoSess).parse(checkPersonWithSession - "session")
```
