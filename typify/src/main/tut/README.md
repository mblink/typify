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

```tut:silent
import shapeless.HNil
import shapeless.syntax.singleton._
import typify.{Typify, Parsed, ParseError}
import typify.convert._
import typify.convert.syntax._
```

Now we can create an instance of Typify  by specifying the failure type we will use and
the type we will parse from.

Typify currently includes support for parsing from Any, circe, and play json.
New types can be added by implementing the CanParse typeclass for your desired source type.

Let's use Any for this example.

```tut
import typify.parsedany._

case class Fail(reason: String)

val tp = new Typify[Fail, Parsed[Any]]
```

We also need to define an implicit function to convert a typify.ParseError to our failure type.

ParseError looks like this

```scala
case class ParseError(key: String, error: String)
```

```tut
implicit val parse2Error = (p: Parsed[Any], pe: ParseError) => Fail(s"${pe.key} - ${pe.error}")
```

Now we can define some validation functions.
Let's validate an email, an age and an optional session id.

```tut
import scalaz.NonEmptyList
import scalaz.syntax.either._
import scalaz.syntax.nel._
import scalaz.syntax.validation._

val checkEmail = Typify.validate((s: String) =>
  s.right[NonEmptyList[Fail]]
   .ensure(Fail("Email is invalid").wrapNel)(_.contains("@"))
   .validation)

val checkAge = Typify.validate((i: Int) =>
  i.right[NonEmptyList[Fail]]
   .ensure(Fail("Too young").wrapNel)(_ > 21)
   .validation)

val checkSessId = Typify.optional((i: Int) =>
  i.right[NonEmptyList[Fail]]
   .ensure(Fail("Invalid session id").wrapNel)(_ > 3000)
   .validation)
```

Now we can define in which fields to look for these values under our source value as follows.

```tut
val checkPerson = 'email ->> checkEmail :: 'age ->> checkAge :: 'session ->> checkSessId :: HNil
```

From here we are able to parse a person out of Any using our Typify instance.

```tut
import tp.syntax._

val passes: Any = Map("email" -> "foo@bar", "age" -> 22, "session" -> 77777, 3 -> "junk")
val passesNoSess: Any = Map("email" -> "foo@bar", "age" -> 22, 500L -> "extra doesnt matter")
val failsAtParse: Any = 33
val failsAtValidation: Any = Map("email" -> "foo", "session" -> 77777)

val passed = Parsed.init(passes).parse(checkPerson)
val passedNoSess = Parsed.init(passesNoSess).parse(checkPerson)
val failedAtParse = Parsed.init(failsAtParse).parse(checkPerson)
val failedAtValidation = Parsed.init(failsAtValidation).parse(checkPerson)
```

Note that a successful validation returns an HList. We can easily convert it to a compatible case
class. Field order is not important, and supersets of target types are allowed.

These conversions are type safe. Attempting to convert to a case class that requires fields which
are not present on a given HList will fail at compile time.

```tut
case class Person(age: Int, email: String)
case class PersonWithSession(session: Option[Int], email: String, age: Int)

passed.map(_.convertTo[Person])
passed.map(_.convertTo[PersonWithSession])
passedNoSess.map(_.convertTo[Person])
passedNoSess.map(_.convertTo[PersonWithSession])
```

Because our validation rules and results are both simply HLists, we can use HList and record
operations to compose rules, and do partial validation.

```tut
import shapeless.record._

val checkSessId = ((i: Int) =>
  i.right[NonEmptyList[Fail]]
   .ensure(Fail("Invalid session id").wrapNel)(_ > 3000)
   .validation)

val checkSessM = Typify.validate(checkSessId)
val checkSessO = Typify.optional(checkSessId)

val checkPerson = 'email ->> checkEmail :: 'age ->> checkAge :: 'session ->> checkSessO :: HNil
val checkPersonWithSession = (checkPerson - 'session) + ('session ->> checkSessM)

val passed = Parsed.init(passes).parse(checkPersonWithSession)
val failedNoSession = Parsed.init(passesNoSess).parse(checkPersonWithSession)
val passedPartialSession = Parsed.init(passesNoSess).parse(checkPersonWithSession - 'session)

case class PersonRequireSession(session: Int, email: String, age: Int)

passed.map(_.convertTo[PersonRequireSession])
failedNoSession.map(_.convertTo[PersonRequireSession])
passedPartialSession.map(_ + ('session ->> 7777)).map(_.convertTo[PersonRequireSession])
```
