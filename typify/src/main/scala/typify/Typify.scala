package typify

import cats.data.ValidatedNel
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.validated._
import shapeless.{::, HList, HNil, Poly2, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist.RightFolder

object Parsed {
  def top[A](cursor: Cursor[A], root: Vector[String]): Cursor[A] =
    root.foldLeft(cursor)(_.downField(_))

  def top[A: Generic](value: A, root: Vector[String]): Cursor[A] =
    top(Cursor.top(value), root)
}

case class ParseError(message: String)

trait CanParse[T, P] {
  def apply(cursor: Cursor[P]): ValidatedNel[ParseError, T]
  def parse(key: String, cursor: Cursor[P]): ValidatedNel[ParseError, T] = apply(cursor.downField(key))
}

object Typify {
  def validate[L, P, A, B](v: (String, A, Cursor[P]) => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[A, P]): KPV[P, L, B] =
    (k: String) => (c0: Cursor[P]) => ap(c0.downField(k))(c => cp(c).fold(_.map(e2l(c, _)).invalid[B], v(k, _, c)))

  def validate[L, P, A, B](v: A => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[A, P]): KPV[P, L, B] =
    validate((_: String, a: A, _: Cursor[P]) => v(a))

  def validate[L, P, A, B](v: (String, A) => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[A, P]): KPV[P, L, B] =
    validate((s: String, p: A, _: Cursor[P]) => v(s, p))

  def optional[L, P, A, B](v: (String, A, Cursor[P]) => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[Option[A], P]): KPV[P, L, Option[B]] =
    (k: String) => (_: Cursor[P]).downField(k) match {
      case Cursor.Failed(_, _) => Option.empty[B].validNel[L]
      case c => cp(c).fold(_.map(e2l(c, _)).invalid[Option[B]], _.traverse(v(k, _, c)))
    }

  def optional[L, P, A, B](v: A => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[Option[A], P]): KPV[P, L, Option[B]] =
    optional((_: String, p: A, _: Cursor[P]) => v(p))

  def optional[L, P, A, B](v: (String, A) => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[Option[A], P]): KPV[P, L, Option[B]] =
    optional((s: String, p: A, _: Cursor[P]) => v(s, p))
}

class Typify[L, P] {
  type PV[A] = typify.PV[P, L, A]
  type KPV[A] = typify.KPV[P, L, A]

  lazy val pvHNil: PV[HNil] = (_: Cursor[P]) => HNil.validNel[L]

  object foldPV extends Poly2 {
    private def runA[A, B, O <: HList](a: PV[A], acc: PV[O])(f: A => B): PV[B :: O] =
      (c: Cursor[P]) => ap(a(c))(v => (v, acc(c)).mapN((x, y) => f(x) :: y))

    implicit def PV[O <: HList, A]: Case.Aux[PV[A], PV[O], PV[A :: O]] =
      at(runA(_, _)(identity[A] _))

    implicit def labelledPV[O <: HList, K <: Symbol, A]: Case.Aux[FieldType[K, PV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => runA(a, acc)(field[K](_)))

    implicit def labelledKPV[O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => runA(a(k.value.name), acc)(field[K](_)))

    implicit def nested[O <: HList, K <: Symbol, I <: HList, FR, IR <: HList](
      implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, FR],
      ev: FR <:< PV[IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PV[O], PV[FieldType[K, IR] :: O]] =
      at((in, acc) => (c: Cursor[P]) =>
        (rf(in, pvHNil)(c.downField(k.value.name)), acc(c)).mapN((x, y) => field[K](x) :: y))
  }

  object syntax {
    implicit class HLOps(c: Cursor[P]) {
      def parse[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R]
      ): ValidatedNel[L, R] =
        rf(in, pvHNil)(c)
    }
  }
}
