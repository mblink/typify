package scala.scalajs.js.typify

import scala.scalajs.js
import scala.scalajs.js.Dynamic
import typify.{CanParse, Parsed, ParseError}
import scala.reflect.ClassTag
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
    def as(d: Dynamic)(implicit ct: ClassTag[Option[Dynamic]]) = {
      Option(d).filterNot(js.isUndefined).successNel[ParseError]
    }

    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Option[Dynamic]]) =
      js.isUndefined(d).fold(None.successNel[ParseError],
        nf(d.selectDynamic)(k)
          .flatMap(as(_).disjunction)
          .orElse(None.right[NonEmptyList[ParseError]])
          .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Option[Dynamic]")))
          .validation)
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
}
