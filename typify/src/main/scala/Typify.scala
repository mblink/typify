package typify

import annotation.implicitNotFound

import scala.reflect.ClassTag
import scalaz.Leibniz.===
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scalaz.ValidationNel
import shapeless.{::, HList, HNil, Poly2, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist.RightFolder

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
  lazy val pvHNil: PV[HNil] = (_: P) => HNil.successNel[L]

  object foldPV extends Poly2 {
    implicit def PV[O <: HList, A]: Case.Aux[PV[A], PV[O], PV[A :: O]] =
      at((a, acc) => (p: P) => (a(p) |@| acc(p))(_ :: _))

    implicit def labelledPV[O <: HList, K <: Symbol, A]: Case.Aux[FieldType[K, PV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => (p: P) => (a(p) |@| acc(p))((x, y) => field[K](x) :: y))

    implicit def labelledKPV[O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => (p: P) => (a(k.value.name)(p) |@| acc(p))((x, y) => field[K](x) :: y))

    implicit def nested[O <: HList, K <: Symbol, I <: HList, FR, IR <: HList](
      implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, FR],
      ev: FR <:< PV[IR]
    ): Case.Aux[FieldType[K, I], PV[O], PV[FieldType[K, IR] :: O]] =
      at[FieldType[K, I], PV[O]]((in, acc) => (p: P) =>
        (rf(in, pvHNil).apply(p) |@| acc(p))((x, y) => field[K](x) :: y))
  }

  type PV[A] = P => ValidationNel[L, A]
  type KPV[A] = String => P => ValidationNel[L, A]

  object syntax {

    implicit class HLOps(p: P) {
      def parse[I <: HList, A, R <: HList](in: I)(
        implicit lf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R]
      ): ValidationNel[L, R] = lf(in, pvHNil)(p)

      def parseOption[I <: HList, A, R <: HList, B](in: I)(
        implicit ev: P <:< Parsed[B], rev: Parsed[B] === P, ct: ClassTag[B],
        lf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R],
        e2l: Typify.E2L[L, B], cp: CanParse[Option[B], B]
      ): ValidationNel[L, Option[R]] =
          p.root.foldLeft(cp.as(p.run).disjunction)(
              (r, k) => r.flatMap(_.map(cp.parse(k, _).disjunction)
                                   .getOrElse(none[B].successNel[ParseError].disjunction)))
           .leftMap(_.map(e2l(p, _)))
           .flatMap(_.traverseU(x => new HLOps(rev(Parsed(x))).parse(in)).disjunction)
           .validation
    }
  }
}
