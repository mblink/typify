package typify

import annotation.implicitNotFound
import shapeless.{HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.record._
import shapeless.syntax.DynamicRecordOps
import shapeless.syntax.singleton._
import shapeless.tag.@@
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.syntax.applicative._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.ValidationNel

trait CanParse[T, P] {
  def parse(k: String, p: P)(implicit ct: ClassTag[T]): ValidationNel[ParseError, T]
}

case class ParseError(key: String, error: String)

case class Parsed[A: ClassTag](run: A, root: Seq[String]) {

  def as[T: ClassTag](key: String)(implicit cp: CanParse[T, A], sp: CanParse[A, A]):
  ValidationNel[ParseError, T] =
    root.foldLeft(run.successNel[ParseError].disjunction)((r, k) => r.flatMap(sp.parse(k, _).disjunction))
      .flatMap(cp.parse(key, _).disjunction).validation

  def withRoot(newRoot: Seq[String]): Parsed[A] = Parsed[A](run, newRoot)
}

trait Parser[F, P, A] {
  def apply(p: Parsed[P]): ValidationNel[F, A]
}

trait FieldParser[F, P, A] {
  def apply(k: String, p: Parsed[P]): ValidationNel[F, A]
}

trait BasicParser[F, P, A] {
  def apply(k: String, p: Parsed[P])(implicit cp: CanParse[A, P]): ValidationNel[F, A]
}

object parsedmap {
  trait MCanParse[T] extends CanParse[T, Map[String, Any]] {
    def parse(k: String, p: Map[String, Any])(implicit ct: ClassTag[T]): ValidationNel[ParseError, T] =
      p.get(k).flatMap(x => x match {
        case y: T => Some(y)
        case _ => None
      }).toSuccessNel(ParseError(k, "could not parse"))
  }
  lazy implicit val cpmas = new MCanParse[Map[String, Any]] {}
  lazy implicit val cpms = new MCanParse[String] {}
  lazy implicit val cpmi = new MCanParse[Int] {}
  lazy implicit val cpmos = new MCanParse[Option[String]] {}
  lazy implicit val cpmoi = new MCanParse[Option[Int]] {}
}

class Typify[L, P] {

  def parseBasic[T: ClassTag](err: ParseError => L)(implicit cp: CanParse[T, P], sp: CanParse[P, P]):
  BasicParser[L, P, T] =
    new BasicParser[L, P, T] {
      def apply(k: String, p: Parsed[P])(implicit cp: CanParse[T, P]) =
        p.as[T](k).leftMap(_.map(err))
    }

  object parsers {

    implicit def hnilParser: Parser[L, P, HNil] = new Parser[L, P, HNil] {
      def apply(p: Parsed[P]): ValidationNel[L, HNil] =
        HNil.successNel[L]
    }

    implicit def hconsParser[K <: Symbol, H, T <: HList](
      implicit key: Witness.Aux[K],
      fp: FieldParser[L, P, H], tp: Parser[L, P, T]):
    Parser[L, P, shapeless.::[FieldType[K, H], T]] = new Parser[L, P, shapeless.::[FieldType[K, H], T]] {
      def apply(p: Parsed[P]): ValidationNel[L, shapeless.::[FieldType[K, H], T]] =
          (fp.apply(key.value.name, p).map(field[K](_))
            |@| implicitly[Parser[L, P, T]].apply(p))(_ :: _)
    }

    implicit def caseClassParser[A, R <: HList](implicit
      gen: LabelledGeneric.Aux[A, R],
      reprParser: Lazy[Parser[L, P, R]]
    ): Parser[L, P, A] = new Parser[L, P, A] {
      def apply(p: Parsed[P]): ValidationNel[L, A] = reprParser.value.apply(p).map(gen.from)
    }

    implicit def partialParser[A, F, Pt <: HList, R <: HList, Rm <: HList](implicit
      ffp: FnFromProduct.Aux[Pt => A, F],
      gen: LabelledGeneric.Aux[A, R],
      rma: RemoveAll.Aux[R, Pt, (Pt, Rm)],
      parser: Parser[L, P, Rm]): Parser[L, P, F] = new Parser[L, P, F] {
        def apply(p: Parsed[P]) = parser(p).map(r => ffp(pt => gen.from(rma.reinsert((pt, r)))))
      }
  }

  def validate[A, B](v: A => ValidationNel[L, B])(implicit
                          fp: BasicParser[L, P, A], cp: CanParse[A, P]):
  FieldParser[L, P, B] = new FieldParser[L, P, B] {
    def apply(k: String, p: Parsed[P]) =
      fp.apply(k, p).disjunction.flatMap(v.andThen(_.disjunction)).validation
  }

  def validate[A, B](v: (String, A, Parsed[P]) => ValidationNel[L, B])(implicit
                          fp: BasicParser[L, P, A], cp: CanParse[A, P]):
  FieldParser[L, P, B] = new FieldParser[L, P, B] {
    def apply(k: String, p: Parsed[P]) =
      fp.apply(k, p).disjunction.flatMap(v(k, _, p).disjunction).validation
  }

  def validate[A, B](v: (String, A) => ValidationNel[L, B])(implicit
                          fp: BasicParser[L, P, A], cp: CanParse[A, P]):
  FieldParser[L, P, B] = new FieldParser[L, P, B] {
    def apply(k: String, p: Parsed[P]) =
      fp.apply(k, p).disjunction.flatMap(v(k, _).disjunction).validation
  }

  def apply[A](p: P, root: Seq[String] = Seq())(implicit ct: ClassTag[P], parser: Parser[L, P, A]):
  ValidationNel[L, A] =
    parser(Parsed(p, root))
}
