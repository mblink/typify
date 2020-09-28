package typify

import cats.data.ValidatedNel
import cats.syntax.traverse._
import cats.syntax.validated._
import scala.reflect.ClassTag
import shapeless.{HList, HNil}
import shapeless.ops.hlist.RightFolder

object Typify {
  def validate[L, P, A, B](v: (String, A, Cursor[P]) => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[A, P]): KPV[P, L, B] =
    (k: String) => (c0: Cursor[P]) => ap(c0.downField(k))(c => cp(c).fold(_.map(e2l).invalid[B], v(k, _, c)))

  def validate[L, P, A, B](v: A => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[A, P]): KPV[P, L, B] =
    validate((_: String, a: A, _: Cursor[P]) => v(a))

  def validate[L, P, A, B](v: (String, A) => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[A, P]): KPV[P, L, B] =
    validate((s: String, a: A, _: Cursor[P]) => v(s, a))

  def list[L, P, B](v: (String, Cursor[P]) => ValidatedNel[L, B])(implicit ct: ClassTag[P], e2l: E2L[L, P], cp: CanParse[List[P], P]): KPV[P, L, List[B]] =
    validate((s: String, _: List[P], c: Cursor[P]) => typify.parseList(c)(
      f => e2l(ParseError(f, s"Could not be parsed as List[$ct]")).invalidNel[List[B]],
      v(s, _)))

  def list[L, P, B](v: Cursor[P] => ValidatedNel[L, B])(implicit ct: ClassTag[P], e2l: E2L[L, P], cp: CanParse[List[P], P]): KPV[P, L, List[B]] =
    list((_, c: Cursor[P]) => v(c))

  def optional[L, P, A, B](v: (String, A, Cursor[P]) => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[Option[A], P]): KPV[P, L, Option[B]] =
    (k: String) => (_: Cursor[P]).downField(k) match {
      case Cursor.Failed(_, _) => Option.empty[B].validNel[L]
      case c => cp(c).fold(_.map(e2l).invalid[Option[B]], _.traverse(v(k, _, c)))
    }

  def optional[L, P, A, B](v: A => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[Option[A], P]): KPV[P, L, Option[B]] =
    optional((_: String, a: A, _: Cursor[P]) => v(a))

  def optional[L, P, A, B](v: (String, A) => ValidatedNel[L, B])(implicit e2l: E2L[L, P], cp: CanParse[Option[A], P]): KPV[P, L, Option[B]] =
    optional((s: String, a: A, _: Cursor[P]) => v(s, a))

  def optionalList[L, P, B](v: (String, Cursor[P]) => ValidatedNel[L, B])(implicit ct: ClassTag[P], e2l: E2L[L, P], cp: CanParse[Option[List[P]], P]): KPV[P, L, Option[List[B]]] =
    optional((s: String, _: List[P], c: Cursor[P]) => typify.parseList(c)(
      f => e2l(ParseError(f, s"Could not be parsed as List[$ct]")).invalidNel[List[B]],
      v(s, _)))

  def optionalList[L, P, B](v: Cursor[P] => ValidatedNel[L, B])(implicit ct: ClassTag[P], e2l: E2L[L, P], cp: CanParse[Option[List[P]], P]): KPV[P, L, Option[List[B]]] =
    optionalList((_, c: Cursor[P]) => v(c))
}

class Typify[L, P] {
  type PV[A] = typify.PV[P, L, A]
  type KPV[A] = typify.KPV[P, L, A]

  object syntax {
    implicit class CursorOps(c: Cursor[P]) {
      def as[A](implicit cp: CanParse[A, P], e2l: E2L[L, P]): ValidatedNel[L, A] =
        cp(c).leftMap(_.map(e2l))

      def get[A](k: String)(implicit cp: CanParse[A, P], e2l: E2L[L, P]): ValidatedNel[L, A] =
        cp.parse(k, c).leftMap(_.map(e2l))

      def parse[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R]
      ): ValidatedNel[L, R] =
        rf(in, pvHNil)(c)

      def parseOption[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R],
        cpop: CanParse[Option[P], P],
        e2l: E2L[L, P]
      ): ValidatedNel[L, Option[R]] =
        c match {
          case f @ Cursor.Failed(_, _) if f.history.failedOp.exists(CursorOp.isDownField) => None.validNel[L]
          case _ => cpop(c).leftMap(_.map(e2l)).andThen(_.traverse(x =>
            c.replace(x, Some(c), CursorOp.WithFocus((_: P) => x)).parse(in)))
        }

      def parseList[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R],
        e2l: E2L[L, P]
      ): ValidatedNel[L, List[R]] =
        typify.parseList(c)(
          f => e2l(ParseError(f, s"Could not be interpreted as List")).invalidNel[List[R]],
          rf(in, pvHNil)(_))
    }
  }
}
