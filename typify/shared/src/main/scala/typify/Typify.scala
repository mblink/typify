package typify

import cats.data.ValidatedNel
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.validated._
import scala.reflect.ClassTag
import shapeless.{::, HList, HNil, Poly2, Witness}
import shapeless.labelled.{field, FieldType}
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

  lazy val pvHNil: PV[HNil] = (_: Cursor[P]) => HNil.validNel[L]

  object foldPV extends Poly2 {
    private def runA[A, B, O <: HList](a: PV[A], acc: PV[O])(f: A => B): PV[B :: O] =
      (c: Cursor[P]) => (a(c), acc(c)).mapN((x, y) => f(x) :: y)

    implicit def PV[O <: HList, A]: Case.Aux[PV[A], PV[O], PV[A :: O]] =
      at(runA(_, _)(identity[A] _))

    implicit def labelledPV[O <: HList, K, A]: Case.Aux[FieldType[K, PV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => runA(a, acc)(field[K](_)))

    implicit def labelledKPVSym[O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => runA(a(k.value.name), acc)(field[K](_)))

    implicit def labelledKPVStr[O <: HList, K <: String, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => runA(a(k.value), acc)(field[K](_)))

    implicit def listOfPVSym[O <: HList, K <: Symbol, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, listOf[PV[A]]], PV[O], PV[FieldType[K, List[A]] :: O]] =
      at((in, acc) => (c: Cursor[P]) =>
        (typify.parseList(c.downField(k.value.name))(
          f => e2l(ParseError(f, "Could not be interpreted as List")).invalidNel[List[A]],
          in.run),
        acc(c)).mapN((x, y) => field[K](x) :: y))

    implicit def listOfPVStr[O <: HList, K <: String, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, listOf[PV[A]]], PV[O], PV[FieldType[K, List[A]] :: O]] =
      at((in, acc) => (c: Cursor[P]) =>
        (typify.parseList(c.downField(k.value))(
          f => e2l(ParseError(f, "Could not be interpreted as List")).invalidNel[List[A]],
          in.run),
        acc(c)).mapN((x, y) => field[K](x) :: y))

    implicit def listOfHList[O <: HList, K, I <: HList, FR, IR <: HList](
      implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, FR],
      ev: FR <:< PV[IR],
      lpv: Case.Aux[FieldType[K, listOf[PV[IR]]], PV[O], PV[FieldType[K, List[IR]] :: O]]
    ): Case.Aux[FieldType[K, listOf[I]], PV[O], PV[FieldType[K, List[IR]] :: O]] =
      at((in, acc) => lpv(field[K](listOf(rf(in.run, pvHNil)(_))), acc))

    implicit def nestedSym[O <: HList, K <: Symbol, I <: HList, FR, IR <: HList](
      implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, FR],
      ev: FR <:< PV[IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PV[O], PV[FieldType[K, IR] :: O]] =
      at((in, acc) => (c: Cursor[P]) =>
        (rf(in, pvHNil)(c.downField(k.value.name)), acc(c)).mapN((x, y) => field[K](x) :: y))

    implicit def nestedStr[O <: HList, K <: String, I <: HList, FR, IR <: HList](
      implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, FR],
      ev: FR <:< PV[IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PV[O], PV[FieldType[K, IR] :: O]] =
      at((in, acc) => (c: Cursor[P]) =>
        (rf(in, pvHNil)(c.downField(k.value)), acc(c)).mapN((x, y) => field[K](x) :: y))
  }

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
