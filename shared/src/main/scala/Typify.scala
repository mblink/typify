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
import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.Leibniz.===
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.ValidationNel

@implicitNotFound(msg = "Cannot find CanParse from ${P} to ${T}")
trait CanParse[T, P] {
  def parse(k: String, p: P)(implicit ct: ClassTag[T]): ValidationNel[ParseError, T]
  def as(p: P)(implicit ct: ClassTag[T]): ValidationNel[ParseError, T]
}

case class ParseError(key: String, error: String)

case class Parsed[A: ClassTag](run: A, root: Seq[String]) {

  def as[T: ClassTag](key: String)(implicit cp: CanParse[T, A], sp: CanParse[A, A]):
  ValidationNel[ParseError, T] =
    root.foldLeft(run.successNel[ParseError].disjunction)((r, k) => r.flatMap(sp.parse(k, _).disjunction))
      .flatMap(cp.parse(key, _).disjunction).validation

  def withRoot(newRoot: Seq[String]): Parsed[A] = Parsed[A](run, newRoot)
}

@implicitNotFound(msg = "Cannot find Parser from ${P} to ${F} \\/ ${A}")
trait Parser[F, P, A] {
  def apply(p: Parsed[P]): ValidationNel[F, A]
}

@implicitNotFound(msg = "Cannot find FieldParser from ${P} to ${A}")
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
    def as(p: Map[String, Any])(implicit ct: ClassTag[T]): ValidationNel[ParseError, T] =
      ParseError("_root_", "Map[String, Any] cannot be converted at root").failureNel[T]
  }
  lazy implicit val cpmas = new MCanParse[Map[String, Any]] {}
  lazy implicit val cpomas = new MCanParse[Option[Map[String, Any]]] {}
  lazy implicit val cpms = new MCanParse[String] {}
  lazy implicit val cpmi = new MCanParse[Int] {}
  lazy implicit val cpml = new MCanParse[Long] {}
  lazy implicit val cpmos = new MCanParse[Option[String]] {}
  lazy implicit val cpmoi = new MCanParse[Option[Int]] {}
  lazy implicit val cpmol = new MCanParse[Option[Long]] {}
}

class Typify[L, P] { typify =>
  type E2L = (Parsed[P], ParseError) => L

  def parseBasic[T: ClassTag](err: ParseError => L)(
                              implicit cp: CanParse[T, P], sp: CanParse[P, P], osp: CanParse[Option[P], P]):
  BasicParser[L, P, T] =
    new BasicParser[L, P, T] {
      def apply(k: String, p: Parsed[P])(implicit cp: CanParse[T, P]) =
        p.as[T](k).leftMap(_.map(err))
    }

  def parseBasic[T: ClassTag](err: (Parsed[P], ParseError) => L)(
                              implicit cp: CanParse[T, P], sp: CanParse[P, P], osp: CanParse[Option[P], P]):
  BasicParser[L, P, T] =
    new BasicParser[L, P, T] {
      def apply(k: String, p: Parsed[P])(implicit cp: CanParse[T, P]) =
        p.as[T](k).leftMap(_.map(err(p, _)))
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

    implicit def caseClassParserO[A, B, R <: HList](implicit
      gen: LabelledGeneric.Aux[B, R],
      reprParser: Lazy[Parser[L, P, R]],
      cp: CanParse[Option[P], P],
      e2l: E2L,
      ct: ClassTag[P],
      ev: A === Option[B]
    ): Parser[L, P, Option[B]] = new Parser[L, P, Option[B]] {
      def apply(p: Parsed[P]): ValidationNel[L, Option[B]] =
        cp.as(p.run).disjunction
          .leftMap(_.map(e2l(p, _)))
          .flatMap(_.map(op => reprParser.value.apply(p.copy(run = op)).map(gen.from).disjunction)
          .sequenceU)
          .validation
    }

    implicit def partialParser[A, F, Pt <: HList, R <: HList, Rm <: HList](implicit
      ffp: FnFromProduct.Aux[Pt => A, F],
      gen: LabelledGeneric.Aux[A, R],
      rma: RemoveAll.Aux[R, Pt, (Pt, Rm)],
      parser: Parser[L, P, Rm]): Parser[L, P, F] = new Parser[L, P, F] {
        def apply(p: Parsed[P]) = parser(p).map(r => ffp(pt => gen.from(rma.reinsert((pt, r)))))
      }

    implicit def partialParserO[A, B, F, Pt <: HList, R <: HList, Rm <: HList](implicit
      ev: B === Option[F],
      ffp: FnFromProduct.Aux[Pt => A, F],
      gen: LabelledGeneric.Aux[A, R],
      e2l: E2L,
      cp: CanParse[Option[P], P],
      ct: ClassTag[P],
      rma: RemoveAll.Aux[R, Pt, (Pt, Rm)],
      parser: Parser[L, P, Rm]): Parser[L, P, Option[F]] = new Parser[L, P, Option[F]] {
        def apply(p: Parsed[P]) =
          cp.as(p.run).disjunction
            .leftMap(_.map(e2l(p, _)))
            .flatMap(_.map(op => parser(p.copy(run = op))
                                   .map(r => ffp(pt => gen.from(rma.reinsert((pt, r)))))
                                   .disjunction)
                      .sequenceU)
            .validation
      }

    implicit def opF[A](implicit ps: Parser[L, P, Option[A]], ct: ClassTag[P], e2l: E2L,
                          cpo: CanParse[Option[P], P], cp: CanParse[P, P]) =
      typify.validate[Option[P], Option[A]]((k: String, op: Option[P], p: Parsed[P]) =>
        typify[Option[A]](p.run, p.root ++ Seq(k)))

    implicit def pF[A](implicit ps: Parser[L, P, A], ct: ClassTag[P], e2l: E2L,
                         cpo: CanParse[Option[P], P], cp: CanParse[P, P]) =
      typify.validate[P, A]((k: String, ip: P, p: Parsed[P]) => typify[A](p.run, p.root ++ Seq(k)))
  }

  def validate[A, B](v: A => ValidationNel[L, B])(implicit ct: ClassTag[A],
                          e2l: E2L, cp: CanParse[A, P],
                          cpp: CanParse[P, P]):
  FieldParser[L, P, B] = new FieldParser[L, P, B] {
    def apply(k: String, p: Parsed[P]) =
      p.as[A](k).leftMap(_.map(e2l(p, _))).disjunction.flatMap(v.andThen(_.disjunction)).validation
  }

  def validate[A, B](v: (String, A, Parsed[P]) => ValidationNel[L, B])(implicit
                          ct: ClassTag[A], e2l: E2L,
                          cp: CanParse[A, P], cpp: CanParse[P, P]):
  FieldParser[L, P, B] = new FieldParser[L, P, B] {
    def apply(k: String, p: Parsed[P]) =
      p.as[A](k).leftMap(_.map(e2l(p, _))).disjunction.flatMap(v(k, _, p).disjunction).validation
  }

  def validate[A, B](v: (String, A) => ValidationNel[L, B])(implicit
                          ct: ClassTag[A], e2l: E2L,
                          cp: CanParse[A, P], cpp: CanParse[P, P]):
  FieldParser[L, P, B] = new FieldParser[L, P, B] {
    def apply(k: String, p: Parsed[P]) =
      p.as[A](k).leftMap(_.map(e2l(p, _))).disjunction.flatMap(v(k, _).disjunction).validation
  }

  def apply[A](p: P, root: Seq[String] = Seq())(implicit ct: ClassTag[P], parser: Parser[L, P, A]):
  ValidationNel[L, A] =
    parser(Parsed(p, root))
}
