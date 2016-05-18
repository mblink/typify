package typify

import annotation.implicitNotFound

import scala.reflect.ClassTag
import scalaz.Leibniz.===
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.{NonEmptyList, ValidationNel}
import shapeless.{::, HList, HNil, Poly2, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist.{Prepend, LeftFolder}

@implicitNotFound(msg = "Cannot find CanParse from ${P} to ${T}")
trait CanParse[T, P] {
  def parse(k: String, p: P)(implicit ct: ClassTag[T]): ValidationNel[ParseError, T]
  def as(p: P)(implicit ct: ClassTag[T]): ValidationNel[ParseError, T]
}

case class ParseError(key: String, error: String)

case class Parsed[A: ClassTag](run: A, root: Seq[String] = Seq()) {

  def as[T: ClassTag](key: String)(implicit cp: CanParse[T, A], sp: CanParse[A, A]):
  ValidationNel[ParseError, T] =
    root.foldLeft(run.successNel[ParseError].disjunction)(
        (r, k) => r.flatMap(sp.parse(k, _).disjunction))
      .flatMap(cp.parse(key, _).disjunction).validation

  def to[T: ClassTag](implicit cp: CanParse[T, A], sp: CanParse[A, A]):
  ValidationNel[ParseError, T] =
    root.foldLeft(run.successNel[ParseError].disjunction)(
        (r, k) => r.flatMap(sp.parse(k, _).disjunction))
      .flatMap(cp.as(_).disjunction).validation

  def withRoot(newRoot: Seq[String]): Parsed[A] = Parsed[A](run, newRoot)
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

object Typify {
  type E2L[L, P] = (Parsed[P], ParseError) => L

  def validate[L, P, A, B](v: A => ValidationNel[L, B])(implicit ct: ClassTag[A],
                          e2l: E2L[L, P], cp: CanParse[A, P],
                          cpp: CanParse[P, P]):
  String => Parsed[P] => ValidationNel[L, B] =
    (k: String) => (p: Parsed[P]) =>
      p.as[A](k).leftMap(_.map(e2l(p, _))).disjunction.flatMap(v.andThen(_.disjunction)).validation

  def validate[L, P, A, B](v: (String, A, Parsed[P]) => ValidationNel[L, B])(implicit
                          ct: ClassTag[A], e2l: E2L[L, P],
                          cp: CanParse[A, P], cpp: CanParse[P, P]):
  String => Parsed[P] => ValidationNel[L, B] =
    (k: String) => (p: Parsed[P]) =>
      p.as[A](k).leftMap(_.map(e2l(p, _))).disjunction.flatMap(v(k, _, p).disjunction).validation

  def validate[L, P, A, B](v: (String, A) => ValidationNel[L, B])(implicit
                          ct: ClassTag[A], e2l: E2L[L, P],
                          cp: CanParse[A, P], cpp: CanParse[P, P]):
  String => Parsed[P] => ValidationNel[L, B] =
    (k: String) => (p: Parsed[P]) =>
      p.as[A](k).leftMap(_.map(e2l(p, _))).disjunction.flatMap(v(k, _).disjunction).validation

  def optional[L, P, A, B](v: A => ValidationNel[L, B])(implicit ct: ClassTag[Option[A]],
                          e2l: E2L[L, P], cp: CanParse[Option[A], P],
                          cpp: CanParse[P, P]):
  String => Parsed[P] => ValidationNel[L, Option[B]] =
    (k: String) => (p: Parsed[P]) =>
      p.as[Option[A]](k).leftMap(_.map(e2l(p, _))).disjunction
       .flatMap(_.map(v.andThen(_.disjunction)).sequenceU).validation

  def optional[L, P, A, B](v: (String, A, Parsed[P]) => ValidationNel[L, B])(implicit
                          ct: ClassTag[Option[A]], e2l: E2L[L, P],
                          cp: CanParse[Option[A], P], cpp: CanParse[P, P]):
  String => Parsed[P] => ValidationNel[L, Option[B]] =
    (k: String) => (p: Parsed[P]) =>
      p.as[Option[A]](k).leftMap(_.map(e2l(p, _))).disjunction
        .flatMap(_.map(v(k, _, p).disjunction).sequenceU).validation

  def optional[L, P, A, B](v: (String, A) => ValidationNel[L, B])(implicit
                          ct: ClassTag[Option[A]], e2l: E2L[L, P],
                          cp: CanParse[Option[A], P], cpp: CanParse[P, P]):
  String => Parsed[P] => ValidationNel[L, Option[B]] =
    (k: String) => (p: Parsed[P]) =>
      p.as[Option[A]](k).leftMap(_.map(e2l(p, _))).disjunction
        .flatMap(_.map(v(k, _).disjunction).sequenceU).validation
}

class Typify[L, P] { typify =>

  object foldPV extends Poly2 {
    implicit def hnil[A] =
      at[PV[HNil], PV[A]] {
        (acc, t) => (((p: P) => (t(p) |@| acc(p))(_ :: _)): PV[A :: HNil])
      }

    implicit def khnil[A, K] =
      at[PV[HNil], FieldType[K, PV[A]]] {
        (acc, t) => (((p: P) => (t(p) |@| acc(p))((x, y) =>
          field[K](x) :: y)): PV[FieldType[K, A] :: HNil])
      }

    implicit def default[A, R <: HList, RA <: HList](
        implicit pp: Prepend.Aux[R, A :: HNil, RA]) =
      at[PV[R], PV[A]] {
        (acc, t) => (((p: P) => (acc(p) |@| t(p).map(_ :: HNil))(pp.apply _)): PV[RA])
      }

    def tagConcat[K, A](f: PV[A], p: P): ValidationNel[L, FieldType[K, A] :: HNil] =
      f(p).map(x => field[K](x) :: HNil)

    implicit def kdefault[K, A, R <: HList, RA <: HList](
        implicit pp: Prepend.Aux[R, FieldType[K, A] :: HNil, RA]) =
      at[PV[R], FieldType[K, PV[A]]] {
        (acc, t) => (((p: P) => (acc(p) |@| tagConcat[K, A](t, p))((x, y) =>
          pp.apply(x, y))): PV[RA])
      }

    implicit def pkdefault[K <: Symbol, A, R <: HList, RA <: HList](
        implicit pp: Prepend.Aux[R, FieldType[K, A] :: HNil, RA],
                 k: Witness.Aux[K]) =
      at[PV[R], FieldType[K, KPV[A]]] {
        (acc, t) => (((p: P) => (acc(p) |@| tagConcat[K, A](t(k.value.name), p))(
                      (x, y) => pp.apply(x, y))): PV[RA])
      }
  }

  type PV[A] = P => ValidationNel[L, A]
  type KPV[A] = String => P => ValidationNel[L, A]

  object syntax {

    implicit class HLOps(p: P) {
      def parse[I <: HList, R <: HList](in: I)(implicit
        lf: LeftFolder.Aux[I, PV[HNil], foldPV.type, PV[R]]): ValidationNel[L, R] =
          in.foldLeft(((_: P) => HNil.successNel[L]): PV[HNil])(foldPV).apply(p)

      def parseOption[I <: HList, R <: HList, B](in: I)(implicit
        ev: P <:< Parsed[B], rev: Parsed[B] === P, ct: ClassTag[B],
        lf: LeftFolder.Aux[I, PV[HNil], foldPV.type, PV[R]],
        e2l: Typify.E2L[L, B], cp: CanParse[Option[B], B],
        cpb: CanParse[B, B]): ValidationNel[L, Option[R]] =
          p.root.foldLeft(cp.as(p.run).disjunction)(
              (r, k) => r.flatMap(_.map(cp.parse(k, _).disjunction)
                                   .getOrElse(none[B].successNel[ParseError].disjunction)))
           .leftMap(_.map(e2l(p, _)))
           .flatMap(_.map(x => new HLOps(rev(Parsed(x))).parse(in)).sequenceU.disjunction)
           .validation
    }
  }
}
