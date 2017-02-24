package scala.scalajs.js.typify

import scala.scalajs.js
import scala.scalajs.js.Dynamic
import typify.{CanParse, Parsed, ParseError}
import scala.reflect.ClassTag
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.either._
import scalaz.syntax.nel._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.{\/, NonEmptyList, ValidationNel}

object parsedinstances {
  def nf[A, B](fn: A => B): A => \/[Throwable, B] =
    { a: A => \/.fromTryCatchNonFatal(fn(a)) }

  lazy implicit val cpd = new CanParse[Dynamic, Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[Dynamic]) =
      Option(d).filterNot(js.isUndefined)
        .toSuccessNel(ParseError("_root_", "Could not be interpreted as Dynamic"))

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Dynamic]) =
      nf(d.selectDynamic)(k)
        .flatMap(nf(_.asInstanceOf[Dynamic]))
        .ensure("null encountered")(x => !js.isUndefined(x))
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Dynamic")))
        .validation
  }

  lazy implicit val cpod = new CanParse[Option[Dynamic], Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[Option[Dynamic]]) =
      Option(d).filterNot(js.isUndefined).successNel[ParseError]

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Option[Dynamic]]) =
      nf(d.selectDynamic)(k)
        .flatMap(as(_).disjunction)
        .orElse(None.right[NonEmptyList[ParseError]])
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Option[Dynamic]")))
        .validation
  }

  implicit def cpla[A: ClassTag](implicit cpa: CanParse[A, Dynamic]) =
    new CanParse[List[A], Dynamic] {
      def as(d: Dynamic)(implicit ct: ClassTag[List[A]]) =
      (Option(d).filterNot(js.isUndefined) \/> "null found").map(_ => d)
        .flatMap(nf(_.asInstanceOf[js.Array[Dynamic]].seq.toList))
        .flatMap(_.traverseU(cpa.as(_)).disjunction)
        .leftMap(_ => ParseError("_root_", s"${d} Could not be interpreted as ${ct}").wrapNel)
        .validation

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[List[A]]) =
      nf(d.selectDynamic)(k)
        .flatMap(as(_).disjunction)
        .leftMap(_ => ParseError(k, s"${d} Could not be parsed as ${ct}").wrapNel)
        .validation
  }

  lazy implicit val cps = new CanParse[String, Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[String]) =
      (Option(d).filterNot(js.isUndefined) \/> "null found").map(_ => d)
        .flatMap(nf(_.asInstanceOf[String]))
        .leftMap(_ => ParseError("_root_", "Could not be interpreted as String").wrapNel)
        .validation

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[String]) =
      nf(d.selectDynamic)(k)
        .flatMap(as(_).disjunction)
        .leftMap(_ => ParseError(k, "Could not be parsed as String").wrapNel)
        .validation
  }

  def parseInt(d: Dynamic): \/[Throwable, Int] =
    \/.fromTryCatchNonFatal(d.asInstanceOf[Int])
      .orElse(\/.fromTryCatchNonFatal(d.asInstanceOf[String]).flatMap(_.parseInt.disjunction))

  lazy implicit val cpi = new CanParse[Int, Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[Int]) =
      (Option(d).filterNot(js.isUndefined) \/> "null found").map(_ => d)
        .flatMap(parseInt)
        .leftMap(_ => NonEmptyList(ParseError("_root_", "Could not be interpreted as Int")))
        .validation

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Int]) =
      nf(d.selectDynamic)(k)
        .flatMap(as(_).disjunction)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Int")))
        .validation
  }

  def parseLong(d: Dynamic): \/[Throwable, Long] =
    \/.fromTryCatchNonFatal(d.asInstanceOf[Double].toLong)
      .orElse(\/.fromTryCatchNonFatal(d.asInstanceOf[String]).flatMap(_.parseLong.disjunction))

  lazy implicit val cpl = new CanParse[Long, Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[Long]) =
      (Option(d).filterNot(js.isUndefined) \/> "null found").map(_ => d)
        .flatMap(parseLong)
        .leftMap(_ => NonEmptyList(ParseError("_root_", "Could not be interpreted as Long")))
        .validation

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Long]) =
      nf(d.selectDynamic)(k)
        .flatMap(as(_).disjunction)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Long")))
        .validation
  }

  def parseBoolean(d: Dynamic): \/[Throwable, Boolean] =
    \/.fromTryCatchNonFatal(d.asInstanceOf[Boolean])
      .orElse(\/.fromTryCatchNonFatal(d.asInstanceOf[String]).flatMap(_.parseBoolean.disjunction))

  lazy implicit val cpb = new CanParse[Boolean, Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[Boolean]) =
      (Option(d).filterNot(js.isUndefined) \/> "null found").map(_ => d)
        .flatMap(parseBoolean)
        .leftMap(_ => NonEmptyList(ParseError("_root_", "Could not be interpreted as Boolean")))
        .validation

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Boolean]) =
      nf(d.selectDynamic)(k)
        .flatMap(as(_).disjunction)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Boolean")))
        .validation
  }

  implicit def cpola[A: ClassTag](implicit cpa: CanParse[A, Dynamic]) =
    new CanParse[Option[List[A]], Dynamic] {
      def as(d: Dynamic)(implicit ct: ClassTag[Option[List[A]]]) =
        Option(d).filterNot(js.isUndefined)
          .map(nf(_.asInstanceOf[js.Array[Dynamic]].seq.toList))
          .map(_.flatMap(_.traverseU(cpa.as(_).disjunction)))
          .sequenceU
          .leftMap(_ => NonEmptyList(ParseError("_root_", s"Could not be interpreted as ${ct}")))
          .validation

      def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Option[List[A]]]) =
        nf(d.selectDynamic)(k)
          .toOption
          .right[Throwable]
          .flatMap(_.traverseU(as(_).disjunction))
          .map(_.flatten)
          .leftMap(_ => ParseError(k, s"${d} Could not be parsed as ${ct}").wrapNel)
          .validation
    }

  lazy implicit val cpos = new CanParse[Option[String], Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[Option[String]]) =
      Option(d).filterNot(js.isUndefined).map(nf(_.asInstanceOf[String])).sequenceU
        .leftMap(_ => NonEmptyList(ParseError("_root_", "Could not be interpreted as Option[String]")))
        .validation

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Option[String]]) =
      nf(d.selectDynamic)(k)
        .toOption
        .successNel[ParseError].disjunction
        .flatMap(_.flatMap(as(_).disjunction.sequenceU).sequenceU)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Option[String]")))
        .validation
  }

  lazy implicit val cpoi = new CanParse[Option[Int], Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[Option[Int]]) =
      Option(d).filterNot(js.isUndefined).map(parseInt).sequenceU
        .leftMap(_ => NonEmptyList(ParseError("_root_", "Could not be interpreted as Option[Int]")))
        .validation

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Option[Int]]) =
      nf(d.selectDynamic)(k)
        .toOption
        .successNel[ParseError].disjunction
        .flatMap(_.flatMap(as(_).disjunction.sequenceU).sequenceU)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Option[Int]")))
        .validation
  }

  lazy implicit val cpol = new CanParse[Option[Long], Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[Option[Long]]) =
      Option(d).filterNot(js.isUndefined).map(parseLong).sequenceU
        .leftMap(_ =>
            NonEmptyList(ParseError("_root_", "Could not be interpreted as Option[Long]")))
        .validation

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Option[Long]]) =
      nf(d.selectDynamic)(k)
        .toOption
        .successNel[ParseError].disjunction
        .flatMap(_.flatMap(as(_).disjunction.sequenceU).sequenceU)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Option[Long]")))
        .validation
  }

  lazy implicit val cpob = new CanParse[Option[Boolean], Dynamic] {
    def as(d: Dynamic)(implicit ct: ClassTag[Option[Boolean]]) =
      Option(d).filterNot(js.isUndefined).map(parseBoolean).sequenceU
        .leftMap(_ => NonEmptyList(ParseError("_root_", "Could not be interpreted as Option[Boolean]")))
        .validation

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Option[Boolean]]) =
      nf(d.selectDynamic)(k)
        .toOption
        .successNel[ParseError].disjunction
        .flatMap(_.flatMap(as(_).disjunction.sequenceU).sequenceU)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Option[Boolean]")))
        .validation
  }
}
