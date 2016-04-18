#Typify

Typify is a library for parsing and validating poorly-typed data into well-typed data.

It leverages [scalaz](https://github.com/scalaz/scalaz) for accumulation of parsing/validation errors,
and [shapeless](https://github.com/milessabin/shapeless) for compile-time introspection and added type safety.

The general philosophy behind typify is that parsing and validation should be done according to the target type of a
parsed and validated value, and that attempts to parse or validate on poor types (those that do not have
validations defined) should result in a compile time failure.

Some care was taken to maintain practical flexibility, namely the error type is defined at the call site, as well
as the actual parsing mechanism via a simple typeclass. A parser for Map[String, Any] is included for example purposes,
and sub-projects will be added shortly to add additional parsing backends which leverage other dependencies.

Use requires a minimal amount of one-time setup and is extremely concise from that point on, as illustrated in the
following example.

First import some requirements.

```scala
scala> import typify.Typify
import typify.Typify

scala> import typify.parsers._
import typify.parsers._

scala> import scalaz.syntax.std.boolean._
import scalaz.syntax.std.boolean._

scala> import scalaz.syntax.std.option._
import scalaz.syntax.std.option._

scala> import shapeless.LabelledGeneric
import shapeless.LabelledGeneric

scala> import shapeless.tag
import shapeless.tag

scala> import shapeless.tag.@@
import shapeless.tag.$at$at
```

With this in place, let's set up the base parsers for String and Int, fixed to our error type, in this case String.

```scala
scala>   implicit lazy val sp = stringParser[String, Map[String, Any]](p => s"${p.key}: ${p.error}")
sp: typify.BasicParser[String,Map[String,Any],String] = <lazy>

scala>   implicit lazy val ip = intParser[String, Map[String, Any]](p => s"${p.key} cannot be parsed as int")
ip: typify.BasicParser[String,Map[String,Any],Int] = <lazy>
```

These methods are provided by the typify.parsers._ import. While they are capable of parsing strings and ints from
poorly typed structures, they are typed as BasicParsers and attempting to parse out values that use these primitive
types will result in a compiler error.

In order to associate validation rules with field types and enforce at compile time that we are using
meaningful types for our data, we need to tag our primitives. Let's create some tags and define a User type
that leverages them.

```scala
scala>   trait Email {}
defined trait Email

scala>   trait Age {}
defined trait Age

scala>   case class Person(email: String @@ Email, age: Int @@ Age)
defined class Person
```

```scala
scala>   case class UnsafePerson(email: String, age: Int)
defined class UnsafePerson
```

We now need a shapeless.LabelledGeneric in scope for our type, to enable compile time introspection

```scala
scala>   implicit lazy val genP = LabelledGeneric[Person]
genP: shapeless.LabelledGeneric[Person]{type Repr = shapeless.::[String with shapeless.tag.Tagged[Email] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String with shapeless.tag.Tagged[Email]],shapeless.::[Int with shapeless.tag.Tagged[Age] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int with shapeless.tag.Tagged[Age]],shapeless.HNil]]} = <lazy>
```

And now we define some validation rules for the types we are using, in this case Email and Age respectively.
Note that these are simply functions from the underlying primitive, to a ValidationNel of our chosen failure type
or the tagged target type.

```scala
scala>   implicit lazy val vEmail = Typify.validate[String, Map[String, Any], String, String @@ Email]((e: String) =>
     |     e.contains("@").option(tag[Email](e)).toSuccessNel("invalid email"))
vEmail: typify.FieldParser[String,Map[String,Any],shapeless.tag.@@[String,Email]] = <lazy>

scala>   implicit lazy val vAge = Typify.validate[String, Map[String, Any], Int, Int @@ Age](a =>
     |     (a > 18).option(tag[Age](a)).toSuccessNel("too young"))
vAge: typify.FieldParser[String,Map[String,Any],shapeless.tag.@@[Int,Age]] = <lazy>
```

Here we have said that a valid email is anything that contains an @ symbol, while a valid age is anything over 18.

With this in place, we are now able to parse a Map[String, Any] into a Person instance and validate it along the way.

```scala
  Typify[String, Map[String, Any], Person](Map("email" -> "foo", "age" -> 17))
  Typify[String, Map[String, Any], Person](Map("email" -> "foo@bar"))
  Typify[String, Map[String, Any], Person](Map("age" -> 22))
  Typify[String, Map[String, Any], Person](Map("email" -> "foo@bar", "age" -> 22))
  Typify[String, Map[String, Any], Person](Map("email" -> 2, "age" -> "bar"))
``

The call to Typify is type-parameterized in order from left to right on the failure type, the type we are parsing
from, and the target type for a successful parse/validation.

Note that with this approach, as we build up a collection of validation rules for specific types, we will add

Let's try with a target type that uses primitives for its fields.

```scala
scala>   case class UnsafePerson(email: String, age: Int)
defined class UnsafePerson
```

We cannot "forget" to define validations for any fields on our data types, as doing so will result in a
compiler error.

```scala
scala>   Typify[String, Map[String, Any], UnsafePerson](Map("email" -> "foo@bar", "age" -> 22))
<console>:37: error: could not find implicit value for parameter parser: typify.Parser[String,Map[String,Any],UnsafePerson]
         Typify[String, Map[String, Any], UnsafePerson](Map("email" -> "foo@bar", "age" -> 22))
                                                       ^
```

As we build up more validated types, we will add less and less as we can re-use the types that we have already
defined rules for, and get the validation for free.
