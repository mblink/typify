#Typify

Typify is a library for parsing and validating poorly-typed data into well-typed data.

The biggest distinction between this and others like it, is that Typify associates parsing and validation
rules with the field types on your case classes, and makes it hard for you to use meaningless or primitive
types. In fact, if you haven't added some meaningful types with explicitly defined validations to the
fields of a case class you are validating, you are met with a compiler error.

Here's a quick preview before going further in depth.

```scala
  case class Person(email: String @@ Email, age: Int @@ Age)
  case class UnsafePerson(email: String, age: Int)
  typify[Person](Map("email" -> "foo@bar", "age" -> 22)) // Success(User)
  typify[String @@ Email => Person](Map("age" -> 22)).map(_("foo@bar")) // Success(User)
  typify[UnsafePerson](Map("email" -> "foo@bar", "age" -> 22)) // compile error
```

Typify leverages [scalaz](https://github.com/scalaz/scalaz) for accumulation of parsing/validation errors,
and [shapeless](https://github.com/milessabin/shapeless) for compile-time introspection and added type safety.

The philosophy of the library is that parsing and validation should be done according to the target type of a
parsed and validated value, and that the compiler should enforce that you only work with types which have
validations defined.

Some care was taken to maintain practical flexibility, namely the type used for failures is defined
at the call site, as well as the actual parsing mechanism via a simple typeclass.
A parser for Map[String, Any] is included for example purposes, and sub-projects will be added
shortly to add additional parsing backends which leverage other dependencies.

Use requires a minimal amount of one-time setup and is extremely concise from that point on, as illustrated in the
following example.

First import some requirements.

```scala
import typify.parsedinstances._
import typify.Typify
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import shapeless.LabelledGeneric
import shapeless.tag
import shapeless.tag.@@
```

With this in place, let's create an instance of Typify, specifying the type we will use for failures, and the
type we will parse from.

```scala
scala>   val typify = new Typify[String, Map[String, Any]]
typify: typify.Typify[String,Map[String,Any]] = typify.Typify@4fbcb516
```

We can now create some BasicParsers that will be leveraged to validate. Note that these are insufficient to do
real work, as they are meant only to handle primitive values. We want to enforce that our values are more
meaningful and so they must be extended to create FieldParsers in the following steps.

```scala
scala>   import typify.parsers._
import typify.parsers._

scala>   implicit lazy val sp = typify.stringParser(p => s"${p.key}: ${p.error}")
sp: typify.BasicParser[String,Map[String,Any],String] = <lazy>

scala>   implicit lazy val ip = typify.intParser(p => s"${p.key} cannot be parsed as int")
ip: typify.BasicParser[String,Map[String,Any],Int] = <lazy>
```

With these in scope we can create our FieldParsers. First let's define meaninful types for our fields.

```scala
  trait Email {}
  trait Age {}

  case class Person(email: String @@ Email, age: Int @@ Age)
```

And now for the FieldParsers

```scala
scala>   implicit lazy val vEmail = typify.validate[String, String @@ Email]((e: String) =>
     |     e.contains("@").option(tag[Email](e)).toSuccessNel("invalid email"))
vEmail: typify.FieldParser[String,Map[String,Any],shapeless.tag.@@[String,Email]] = <lazy>

scala>   implicit lazy val vAge = typify.validate[Int, Int @@ Age](a =>
     |     (a > 18).option(tag[Age](a)).toSuccessNel("too young"))
vAge: typify.FieldParser[String,Map[String,Any],shapeless.tag.@@[Int,Age]] = <lazy>
```

Here we have said that a valid email is anything that contains an @ symbol, while a valid age is anything over 18.

We now need a shapeless.LabelledGeneric in scope for our type, to enable compile time introspection

```scala
scala>   implicit lazy val genP = LabelledGeneric[Person]
genP: shapeless.LabelledGeneric[Person]{type Repr = shapeless.::[String with shapeless.tag.Tagged[Email] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("email")],String with shapeless.tag.Tagged[Email]],shapeless.::[Int with shapeless.tag.Tagged[Age] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int with shapeless.tag.Tagged[Age]],shapeless.HNil]]} = <lazy>
```


With this in place, we are now able to parse a Map[String, Any] into a Person instance and validate it along the way.

```scala
scala>   typify[Person](Map("email" -> "foo", "age" -> 17))
res1: scalaz.ValidationNel[String,Person] = Failure(NonEmpty[invalid email,too young])

scala>   typify[Person](Map("email" -> "foo@bar"))
res2: scalaz.ValidationNel[String,Person] = Failure(NonEmpty[age cannot be parsed as int])

scala>   typify[Person](Map("age" -> 22))
res3: scalaz.ValidationNel[String,Person] = Failure(NonEmpty[email: could not parse])

scala>   typify[Person](Map("email" -> "foo@bar", "age" -> 22))
res4: scalaz.ValidationNel[String,Person] = Success(Person(foo@bar,22))

scala>   typify[Person](Map("email" -> 2, "age" -> "bar"))
res5: scalaz.ValidationNel[String,Person] = Failure(NonEmpty[email: could not parse,age cannot be parsed as int])
```

If we have a partial structure and can pull remaining values from elsewhere, partial parsing is also supported

```scala
scala>   typify[Int @@ Age => Person](Map("email" -> "foo@bar")).map(_(tag[Age](25)))
res6: scalaz.Validation[scalaz.NonEmptyList[String],Person] = Success(Person(foo@bar,25))
```

Note that with this approach, as we build up a collection of validation rules for specific types, we will add them
less and less often, and re-use simply by tagging fields with previously validated types.

Let's try with a target type that uses primitives for its fields.

```scala
  case class UnsafePerson(email: String, age: Int)
  implicit lazy val genUP = LabelledGeneric[UnsafePerson]
```

We cannot "forget" to define validations for any fields on our data types, as doing so will result in a
compiler error.

```scala
scala>   Typify[String, Map[String, Any], UnsafePerson](Map("email" -> "foo@bar", "age" -> 22))
<console>:42: error: not found: value Typify
         Typify[String, Map[String, Any], UnsafePerson](Map("email" -> "foo@bar", "age" -> 22))
         ^
```
