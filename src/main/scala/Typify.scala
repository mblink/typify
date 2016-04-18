package typify

import shapeless.{HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.record._
import shapeless.syntax.DynamicRecordOps
import shapeless.syntax.singleton._
import shapeless.tag.@@
import scala.reflect.ClassTag
import scalaz.syntax.applicative._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.ValidationNel

trait CanParse[T, P]

case class ParseError(key: String, error: String)

trait Parsed[A] {

  def as[T: ClassTag](p: A, key: String): ValidationNel[ParseError, T]
}

trait Parser[L, P, A] {
  def apply(p: P): ValidationNel[L, A]
}

trait FieldParser[L, P, A] {
  def apply(k: String, p: P): ValidationNel[L, A]
}

trait BasicParser[L, P, A] {
  def apply(k: String, p: P)(implicit cp: CanParse[A, P]): ValidationNel[L, A]
}

object parsedinstances {
  lazy implicit val cpms = new CanParse[String, Map[String, Any]] {}
  lazy implicit val cpmi = new CanParse[Int, Map[String, Any]] {}

  implicit object ParsedMap extends Parsed[Map[String, Any]] {

    def as[T: ClassTag](p: Map[String, Any], key: String): ValidationNel[ParseError, T] =
      p.get(key).flatMap(x => x match {
        case y: T => Some(y)
        case _ => None
      }).toSuccessNel(ParseError(key, "could not parse"))
  }
}

class Typify[L, P: Parsed] {

  def parseBasic[T: ClassTag](err: ParseError => L)(implicit cp: CanParse[T, P]):
  BasicParser[L, P, T] =
    new BasicParser[L, P, T] {
      def apply(k: String, p: P)(implicit cp: CanParse[T, P]) =
        implicitly[Parsed[P]].as[T](p, k).leftMap(_.map(err))
    }

  object parsers {

    implicit def hnilParser[L, P: Parsed]: Parser[L, P, HNil] = new Parser[L, P, HNil] {
      def apply(p: P): ValidationNel[L, HNil] =
        HNil.successNel[L]
    }

    implicit def hconsParser[L, K <: Symbol, H, T <: HList, P: Parsed](
      implicit key: Witness.Aux[K],
      fp: FieldParser[L, P, H], tp: Parser[L, P, T]):
    Parser[L, P, shapeless.::[FieldType[K, H], T]] = new Parser[L, P, shapeless.::[FieldType[K, H], T]] {
      def apply(p: P): ValidationNel[L, shapeless.::[FieldType[K, H], T]] =
          (fp.apply(key.value.name, p).map(field[K](_))
            |@| implicitly[Parser[L, P, T]].apply(p))(_ :: _)
    }

    implicit def caseClassParser[L, A, P: Parsed, R <: HList](implicit
      gen: LabelledGeneric.Aux[A, R],
      reprParser: Lazy[Parser[L, P, R]]
    ): Parser[L, P, A] = new Parser[L, P, A] {
      def apply(p: P): ValidationNel[L, A] = reprParser.value.apply(p).map(gen.from)
    }

    implicit def partialParser[L, A, F, P: Parsed, Pt <: HList, R <: HList, Rm <: HList](implicit
      ffp: FnFromProduct.Aux[Pt => A, F],
      gen: LabelledGeneric.Aux[A, R],
      rma: RemoveAll.Aux[R, Pt, (Pt, Rm)],
      parser: Parser[L, P, Rm]): Parser[L, P, F] = new Parser[L, P, F] {
        def apply(p: P) = parser(p).map(r => ffp(pt => gen.from(rma.reinsert((pt, r)))))
      }
  }


  def validate[A, B](v: A => ValidationNel[L, B])(implicit
                          fp: BasicParser[L, P, A], cp: CanParse[A, P]):
  FieldParser[L, P, B] = new FieldParser[L, P, B] {
    def apply(k: String, p: P) =
      fp.apply(k, p).disjunction.flatMap(v.andThen(_.disjunction)).validation
  }

  def apply[A](p: P)(implicit parser: Parser[L, P, A]): ValidationNel[L, A] = parser(p)
}
