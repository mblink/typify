package scala.scalajs.js.typify

import scala.scalajs.js
import scala.scalajs.js.Dynamic
import typify.{CanParse, Parsed, ParseError}
import scala.reflect.ClassTag
import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.{\/, NonEmptyList, ValidationNel}

object parsedinstances {
  def nf[A, B](fn: A => B): A => \/[Throwable, B] =
    { a: A => \/.fromTryCatchNonFatal(fn(a)) }

  lazy implicit val cpd = new CanParse[Dynamic, Dynamic] {
    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Dynamic]) =
      nf(d.selectDynamic)(k)
        .flatMap(x => (Option(x) \/> "null found").map(_ => x))
        .flatMap(nf(_.asInstanceOf[Dynamic]))
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Dynamic")))
        .validation
  }

  lazy implicit val cps = new CanParse[String, Dynamic] {
    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[String]) =
      nf(d.selectDynamic)(k)
        .flatMap(x => (Option(x).filterNot(js.isUndefined) \/> "null found").map(_ => x))
        .flatMap(nf(_.asInstanceOf[String]))
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as String")))
        .validation
  }

  def parseInt(d: Dynamic): \/[Throwable, Int] =
    \/.fromTryCatchNonFatal(d.asInstanceOf[Int])
      .orElse(\/.fromTryCatchNonFatal(d.asInstanceOf[String]).flatMap(_.parseInt.disjunction))

  lazy implicit val cpi = new CanParse[Int, Dynamic] {
    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Int]) =
      nf(d.selectDynamic)(k)
        .flatMap(x => (Option(x).filterNot(js.isUndefined) \/> "null found").map(_ => x))
        .flatMap(parseInt)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Int")))
        .validation
  }

  lazy implicit val cpos = new CanParse[Option[String], Dynamic] {
    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Option[String]]) =
      nf(d.selectDynamic)(k)
        .flatMap(x =>
          Option(x).filterNot(js.isUndefined).map(nf(_.asInstanceOf[String])).sequenceU)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Option[String]")))
        .validation
  }

  lazy implicit val cpoi = new CanParse[Option[Int], Dynamic] {
    def parse(k: String, d: Dynamic)(implicit ct: ClassTag[Option[Int]]) =
      nf(d.selectDynamic)(k)
        .flatMap(x =>
          Option(x).filterNot(js.isUndefined).map(parseInt).sequenceU)
        .leftMap(_ => NonEmptyList(ParseError(k, "Could not be parsed as Option[Int]")))
        .validation
  }
}
