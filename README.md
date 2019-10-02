#Typify

Typify is a library for parsing and validating poorly-typed data into well-typed data.

While it currently provides support for circe, json4s, play-json, scalajs, and the Any type,
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
import shapeless.{::, HNil}
import shapeless.syntax.singleton._
import typify.{Typify, Parsed, ParseError}
import typify.convert._
import typify.convert.syntax._
```

Now we can create an instance of Typify  by specifying the failure type we will use and
the type we will parse from.

Typify currently includes support for parsing from Any, circe, play json, json4s, and scalajs Dynamic.
New types can be added by implementing the CanParse typeclass for your desired source type.

Let's use Any for this example.

```scala
scala> import typify.parsedany._
import typify.parsedany._

scala> case class Fail(reason: String)
defined class Fail

scala> val tp = new Typify[Fail, Parsed[Any]]
tp: typify.Typify[Fail,typify.Parsed[Any]] = typify.Typify@414fe88f
```

We also need to define an implicit function to convert a typify.ParseError to our failure type.

ParseError looks like this

```scala
case class ParseError(key: String, error: String)
```

```scala
scala> implicit val parse2Error = (p: Parsed[Any], pe: ParseError) => Fail(s"${pe.key} - ${pe.error}")
parse2Error: (typify.Parsed[Any], typify.ParseError) => Fail = <function2>
```

Now we can define some validation functions.
Let's validate an email, an age and an optional session id.

```scala
scala> import scalaz.NonEmptyList
import scalaz.NonEmptyList

scala> import scalaz.syntax.either._
import scalaz.syntax.either._

scala> import scalaz.syntax.nel._
import scalaz.syntax.nel._

scala> import scalaz.syntax.validation._
import scalaz.syntax.validation._

scala> val checkEmail = Typify.validate((s: String) =>
     |   s.right[NonEmptyList[Fail]]
     |    .ensure(Fail("Email is invalid").wrapNel)(_.contains("@"))
     |    .validation)
checkEmail: String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,String]) = <function1>

scala> val checkAge = Typify.validate((i: Int) =>
     |   i.right[NonEmptyList[Fail]]
     |    .ensure(Fail("Too young").wrapNel)(_ > 21)
     |    .validation)
checkAge: String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Int]) = <function1>

scala> val checkSessId = Typify.optional((i: Int) =>
     |   i.right[NonEmptyList[Fail]]
     |    .ensure(Fail("Invalid session id").wrapNel)(_ > 3000)
     |    .validation)
checkSessId: String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Option[Int]]) = <function1>
```

Now we can define in which fields to look for these values under our source value as follows.

```scala
scala> val checkPerson = 'email ->> checkEmail :: 'age ->> checkAge :: 'session ->> checkSessId :: HNil
checkPerson: shapeless.::[String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,String]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,String])],shapeless.::[String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Int]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Int])],shapeless.::[String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Option[Int]]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Option[Int]])],shapeless.HNil]]] = <function1> :: <function1> :: <function1> :: HNil
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

scala> val passed = Parsed(passes).parse(checkPerson)
passed: scalaz.ValidationNel[Fail,shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int],shapeless.::[Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Option[Int]],shapeless.HNil]]]] = Success(foo@bar :: 22 :: Some(77777) :: HNil)

scala> val passedNoSess = Parsed(passesNoSess).parse(checkPerson)
passedNoSess: scalaz.ValidationNel[Fail,shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int],shapeless.::[Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Option[Int]],shapeless.HNil]]]] = Success(foo@bar :: 22 :: None :: HNil)

scala> val failedAtParse = Parsed(failsAtParse).parse(checkPerson)
failedAtParse: scalaz.ValidationNel[Fail,shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int],shapeless.::[Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Option[Int]],shapeless.HNil]]]] = Failure(NonEmpty[Fail(email - Could not be parsed as java.lang.String),Fail(age - Could not be parsed as Int)])

scala> val failedAtValidation = Parsed(failsAtValidation).parse(checkPerson)
failedAtValidation: scalaz.ValidationNel[Fail,shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int],shapeless.::[Option[Int] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Option[Int]],shapeless.HNil]]]] = Failure(NonEmpty[Fail(Email is invalid),Fail(age - Could not be parsed as Int)])
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
res0: scalaz.Validation[scalaz.NonEmptyList[Fail],Person] = Success(Person(22,foo@bar))

scala> passed.map(_.convertTo[PersonWithSession])
res1: scalaz.Validation[scalaz.NonEmptyList[Fail],PersonWithSession] = Success(PersonWithSession(Some(77777),foo@bar,22))

scala> passedNoSess.map(_.convertTo[Person])
res2: scalaz.Validation[scalaz.NonEmptyList[Fail],Person] = Success(Person(22,foo@bar))

scala> passedNoSess.map(_.convertTo[PersonWithSession])
res3: scalaz.Validation[scalaz.NonEmptyList[Fail],PersonWithSession] = Success(PersonWithSession(None,foo@bar,22))
```

Because our validation rules and results are both simply HLists, we can use HList and record
operations to compose rules, and do partial validation.

```scala
scala> import shapeless.record._
import shapeless.record._

scala> val checkSessId = ((i: Int) =>
     |   i.right[NonEmptyList[Fail]]
     |    .ensure(Fail("Invalid session id").wrapNel)(_ > 3000)
     |    .validation)
checkSessId: Int => scalaz.Validation[scalaz.NonEmptyList[Fail],Int] = <function1>

scala> val checkSessM = Typify.validate(checkSessId)
checkSessM: String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Int]) = <function1>

scala> val checkSessO = Typify.optional(checkSessId)
checkSessO: String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Option[Int]]) = <function1>

scala> val checkPerson = 'email ->> checkEmail :: 'age ->> checkAge :: 'session ->> checkSessO :: HNil
checkPerson: shapeless.::[String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,String]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,String])],shapeless.::[String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Int]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Int])],shapeless.::[String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Option[Int]]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],String => (typify.Parsed[Any] => scalaz.ValidationNel[Fail,Option[Int]])],shapeless.HNil]]] = <function1> :: <function1> :: <function1> :: HNil

scala> val checkPersonWithSession = (checkPerson - 'session) + ('session ->> checkSessM)
checkPersonWithSession: shapeless.::[String => (typify.Parsed[Any] => scalaz.Validation[scalaz.NonEmptyList[Fail],String]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String => (typify.Parsed[Any] => scalaz.Validation[scalaz.NonEmptyList[Fail],String])],shapeless.::[String => (typify.Parsed[Any] => scalaz.Validation[scalaz.NonEmptyList[Fail],Int]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],String => (typify.Parsed[Any] => scalaz.Validation[scalaz.NonEmptyList[Fail],Int])],shapeless.::[String => (typify.Parsed[Any] => scalaz.Validation[scalaz.NonEmptyList[Fail],Int]) with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],String => (typify.Parsed[Any] => scalaz.Validation[scalaz.NonEmptyLi...

scala> val passed = Parsed(passes).parse(checkPersonWithSession)
passed: scalaz.ValidationNel[Fail,shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Int],shapeless.HNil]]]] = Success(foo@bar :: 22 :: 77777 :: HNil)

scala> val failedNoSession = Parsed(passesNoSess).parse(checkPersonWithSession)
failedNoSession: scalaz.ValidationNel[Fail,shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("session")],Int],shapeless.HNil]]]] = Failure(NonEmpty[Fail(session - Could not be parsed as Int)])

scala> val passedPartialSession = Parsed(passesNoSess).parse(checkPersonWithSession - 'session)
passedPartialSession: scalaz.ValidationNel[Fail,shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int],shapeless.HNil]]] = Success(foo@bar :: 22 :: HNil)

scala> case class PersonRequireSession(session: Int, email: String, age: Int)
defined class PersonRequireSession

scala> passed.map(_.convertTo[PersonRequireSession])
res4: scalaz.Validation[scalaz.NonEmptyList[Fail],PersonRequireSession] = Success(PersonRequireSession(77777,foo@bar,22))

scala> failedNoSession.map(_.convertTo[PersonRequireSession])
res5: scalaz.Validation[scalaz.NonEmptyList[Fail],PersonRequireSession] = Failure(NonEmpty[Fail(session - Could not be parsed as Int)])

scala> passedPartialSession.map(_ + ('session ->> 7777)).map(_.convertTo[PersonRequireSession])
res6: scalaz.Validation[scalaz.NonEmptyList[Fail],PersonRequireSession] = Success(PersonRequireSession(7777,foo@bar,22))
```
