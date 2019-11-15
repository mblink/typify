package typify

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scalaz.{NonEmptyList, State, ValidationNel}
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.id._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import shapeless.{::, HList, HNil, Poly1, Poly2, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist.{Mapper, RightFolder}

object Validated {
  def apply[L, A](f: Vector[Op] => (Vector[Op], ValidationNel[L, A])): Validated[L, A] =
    ValidationT(State(f))

  def apply[L, A](f: Vector[Op] => ValidationNel[L, (Vector[Op], A)])(implicit d: DummyImplicit): Validated[L, A] =
    ValidationT(State((ops: Vector[Op]) => f(ops).fold(
      e => (ops, e.failure[A]),
      t => (t._1, t._2.successNel[L]))))

  object syntax {
    implicit class ValidatedOps[L, A](v: Validated[L, A]) {
      def ensure(l: Vector[Op] => NonEmptyList[L])(f: A => Boolean): Validated[L, A] =
        ValidationT(State(v.run(_) |> (t => (t._1, t._2.ensure(l(t._1))(f)))))

      def ensure(l: Vector[Op] => L)(f: A => Boolean)(implicit d: DummyImplicit): Validated[L, A] =
        ensure(l.andThen(NonEmptyList(_)))(f)
    }

    implicit class ValidationNelOps[L, A](v: ValidationNel[L, A]) {
      def ensureV(l: Vector[Op] => NonEmptyList[L])(f: A => Boolean): Validated[L, A] =
        Validated(ops => v.map(ops -> _)).ensure(l)(f)

      def ensureV(l: Vector[Op] => L)(f: A => Boolean)(implicit d: DummyImplicit): Validated[L, A] =
        ensureV(l.andThen(NonEmptyList(_)))(f)
    }
  }
}

sealed trait Op
object Op {
  case class ArrayIndex(index: Int) extends Op
  case class DownField(key: String) extends Op
  case class TypeValue[A](value: A)(implicit val classTag: ClassTag[A]) extends Op

  trait MkArrayIndex[L] {
    def apply[A](a: A, i: Int): Validated[L, A] =
      Validated(ops => (ops :+ ArrayIndex(i), a).successNel[L])
  }

  def arrayIndexL[L]: MkArrayIndex[L] = new MkArrayIndex[L] {}

  def arrayIndex[A](a: A, i: Int): ParsedValidated[A] = arrayIndexL[ParseError](a, i)

  def downField[A](a: A, k: String): ParsedValidated[A] =
    Validated(ops => (ops :+ DownField(k), a).successNel[ParseError])

  def downFieldError[A](k: String)(implicit ct: ClassTag[A]): ParsedValidated[A] =
    Validated(ParseError(_, DownField(k), s"Could not be parsed as $ct").failureNel[(Vector[Op], A)])

  def typeValue[A](a: A)(implicit ct: ClassTag[A]): ParsedValidated[A] =
    Validated(ops => (ops :+ TypeValue(a), a).successNel[ParseError])

  def typeValueError[A](a: Option[A])(implicit ct: ClassTag[A]): ParsedValidated[A] =
    Validated(ParseError(_, TypeValue(a), s"Could not be interpreted as $ct").failureNel[(Vector[Op], A)])
}

case class ParseError(ops: Vector[Op], failedOp: Op, error: String)

case class Parsed[A](run: A, root: Vector[String] = Vector()) { self =>
  def as[T](key: String)(implicit cpt: CanParse[T, A], cpa: CanParse[A, A]): ParsedValidated[T] =
    root.foldLeft(run.point[ParsedValidated])((r, k) => r.flatMap(cpa.parse(k, _))).flatMap(cpt.parse(key, _))

  def to[T](implicit cpt: CanParse[T, A], cpa: CanParse[A, A]): ParsedValidated[T] =
    root.foldLeft(run.point[ParsedValidated])((r, k) => r.flatMap(cpa.parse(k, _))).flatMap(cpt.as(_))

  def withRoot(newRoot: Vector[String]): Parsed[A] = Parsed[A](run, newRoot)
}

@implicitNotFound(msg = "Cannot find CanParse from ${P} to ${T}")
trait CanParse[T, P] {
  def parse(k: String, p: P): ParsedValidated[T]
  def as(p: P): ParsedValidated[T]
}


object Typify {
  def validate[L, P, A, B](v: (String, A, Parsed[P]) => Validated[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    (k: String) => (p: Parsed[P]) =>
      p.as[A](k).leftMap(_.map(e2l(p, _))).flatMap(v(k, _, p)).run(Vector())

  def validate[L, P, A, B](v: (String, A, Parsed[P]) => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P], d: DummyImplicit
  ): KPV[P, L, B] =
    validate((k: String, a: A, p: Parsed[P]) => Validated(ops => v(k, a, p).map(ops -> _)))

  def validate[L, P, A, B](v: A => Validated[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    validate((_: String, p: A, _: Parsed[P]) => v(p))

  def validate[L, P, A, B](v: A => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P], d: DummyImplicit
  ): KPV[P, L, B] =
    validate((_: String, p: A, _: Parsed[P]) => v(p))

  def validate[L, P, A, B](v: (String, A) => Validated[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    validate((s: String, p: A, _: Parsed[P]) => v(s, p))

  def validate[L, P, A, B](v: (String, A) => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P], d: DummyImplicit
  ): KPV[P, L, B] =
    validate((s: String, p: A, _: Parsed[P]) => v(s, p))

  def optional[L, P, A, B](v: (String, A, Parsed[P]) => Validated[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    (k: String) => (p: Parsed[P]) =>
      p.as[Option[A]](k).leftMap(_.map(e2l(p, _))).flatMap(_.traverse(v(k, _, p))).run(Vector())

  def optional[L, P, A, B](v: (String, A, Parsed[P]) => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P], d: DummyImplicit
  ): KPV[P, L, Option[B]] =
    optional((k: String, a: A, p: Parsed[P]) => Validated(ops => v(k, a, p).map(ops -> _)))

  def optional[L, P, A, B](v: A => Validated[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    optional((_: String, p: A, _: Parsed[P]) => v(p))

  def optional[L, P, A, B](v: A => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P], d: DummyImplicit
  ): KPV[P, L, Option[B]] =
    optional((_: String, p: A, _: Parsed[P]) => v(p))

  def optional[L, P, A, B](v: (String, A) => Validated[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    optional((s: String, p: A, _: Parsed[P]) => v(s, p))

  def optional[L, P, A, B](v: (String, A) => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P], d: DummyImplicit
  ): KPV[P, L, Option[B]] =
    optional((s: String, p: A, _: Parsed[P]) => v(s, p))
}

class Typify[L, P] {
  type PV[A] = typify.PV[P, L, A]
  type PVWithoutOps[A] = typify.PVWithoutOps[P, L, A]
  type KPV[A] = typify.KPV[P, L, A]

  lazy val pvHNil: PVWithoutOps[HNil] = (_: Parsed[P]) => HNil.successNel[L]

  object foldPV extends Poly2 {
    private def runA[A, B, O <: HList](a: PV[A], acc: PVWithoutOps[O])(f: A => B): PVWithoutOps[(Vector[Op], B) :: O] =
      (p: Parsed[P]) => a(p) |> (t => (t._2 |@| acc(p))((x, y) => (t._1, f(x)) :: y))

    implicit def PV[O <: HList, A]: Case.Aux[PV[A], PVWithoutOps[O], PVWithoutOps[(Vector[Op], A) :: O]] =
      at(runA(_, _)(identity[A] _))

    implicit def labelledPV[O <: HList, K <: Symbol, A]: Case.Aux[FieldType[K, PV[A]], PVWithoutOps[O], PVWithoutOps[(Vector[Op], FieldType[K, A]) :: O]] =
      at((a, acc) => runA(a, acc)(field[K](_)))

    implicit def labelledKPV[O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[A]], PVWithoutOps[O], PVWithoutOps[(Vector[Op], FieldType[K, A]) :: O]] =
      at((a, acc) => runA(a(k.value.name), acc)(field[K](_)))

    implicit def nested[O <: HList, K <: Symbol, I <: HList, FR, IR <: HList](
      implicit rf: RightFolder.Aux[I, PVWithoutOps[HNil], foldPV.type, FR],
      ev: FR <:< PVWithoutOps[IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PVWithoutOps[O], PVWithoutOps[FieldType[K, IR] :: O]] =
      at((in, acc) => (p: Parsed[P]) =>
        (rf(in, pvHNil)(p.withRoot(p.root :+ k.value.name)) |@| acc(p))((x, y) => field[K](x) :: y))
  }

  object removeOps extends Poly1 {
    implicit def withOps[A]: Case.Aux[(Vector[Op], A), A] = at(_._2)
  }

  object syntax {
    implicit class HLOps(p: Parsed[P]) {
      def parseWithOps[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PVWithoutOps[HNil], foldPV.type, A],
        pvaEv: A <:< PVWithoutOps[R]
      ): ValidationNel[L, R] =
        rf(in, pvHNil)(p)

      def parse[I <: HList, A, R <: HList, S <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PVWithoutOps[HNil], foldPV.type, A],
        pvaEv: A <:< PVWithoutOps[R],
        m: Mapper.Aux[removeOps.type, R, S]
      ): ValidationNel[L, S] =
        rf(in, pvHNil)(p).map(m(_))

      // def parseOptionWithOps[I <: HList, A, R <: HList](in: I)(
      //   implicit rf: RightFolder.Aux[I, PVWithoutOps[HNil], foldPV.type, A],
      //   pvaEv: A <:< PVWithoutOps[R],
      //   e2l: E2L[L, P],
      //   cpop: CanParse[Option[P], P]
      // ): (Vector[Op], ValidationNel[L, Option[R]]) =
      //   p.root.foldLeft(cpop.as(p.run))(
      //       (r, k) => r.flatMap(_.fold(Op.downField(none[P], k))(cpop.parse(k, _))))
      //     .leftMap(_.map(e2l(p, _)))
      //     .flatMap(_.traverse(x => Validated(_ => Parsed(x).parseWithOps(in))))
      //     .run(Vector())

      // def parseOption[I <: HList, A, R <: HList](in: I)(
      //   implicit rf: RightFolder.Aux[I, PVWithoutOps[HNil], foldPV.type, A],
      //   pvaEv: A <:< PVWithoutOps[R],
      //   e2l: E2L[L, P],
      //   cpop: CanParse[Option[P], P]
      // ): ValidationNel[L, Option[R]] =
      //   parseOptionWithOps(in)._2
    }
  }
}
