package typify

import scala.annotation.implicitNotFound
import scalaz.ValidationNel
import scalaz.Validation.FlatMap._
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import shapeless.{::, HList, HNil, Poly2, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist.RightFolder

sealed trait Op
object Op {
  case object Init extends Op
  case class ArrayIndex(index: Int) extends Op
  case class DownField(key: String) extends Op
  case class TypeValue[A](value: A) extends Op
}

case class ParseError[P](parsed: Parsed[P], key: String, error: String)

sealed abstract case class Parsed[A](val root: Vector[String], val ops: Vector[Op], val value: A) { self =>
  type Orig
  val orig: Orig

  private def next0[B](r: Vector[String], o: Vector[Op], f: B): Parsed.Aux[B, Orig] = new Parsed[B](r, o, f) {
    type Orig = self.Orig
    val orig = self.orig
  }

  def withRoot(r: Vector[String]): Parsed.Aux[A, Orig] = next0(r, ops, value)
  def addRoot(s: String): Parsed.Aux[A, Orig] = next0(root :+ s, ops, value)

  @inline def next[B](b: B, os: Vector[Op]): Parsed.Aux[B, Orig] = next0(root, ops ++ os, b)
  @inline def next[B](b: B, o: Op, os: Op*): Parsed.Aux[B, Orig] = next(b, o +: os.toVector)
  @inline def next[B](o: Op.TypeValue[B]): Parsed.Aux[B, Orig] = next(o.value, o)
  @inline def next[B](o1: Op, o2: Op.TypeValue[B]): Parsed.Aux[B, Orig] = next(o2.value, o1, o2)

  def atOrig: Parsed[Orig] = next0(root, Vector(Op.Init), orig)

  def atRoot(implicit cpa: CanParse[A, A]): ValidationNel[ParseError[A], Parsed[A]] =
    root.foldLeft(self.successNel[ParseError[A]])((r, k) => r.flatMap(cpa.parse(k, _)))

  def atRootO(implicit cpa: CanParse[Option[A], A]) = //: ValidationNel[ParseError[A], Parsed[A]] =
    root.foldLeft(cpa.as(self))((r, k) => r.flatMap(p => p.value.fold(
      p.successNel[ParseError[A]])(
      a => cpa.parse(k, p.next0(p.root, p.ops, a)))))

  def as[T](key: String)(implicit cpt: CanParse[T, A], cpa: CanParse[A, A]): ValidationNel[ParseError[A], Parsed[T]] =
    atRoot.flatMap(cpt.parse(key, _))

  def to[T](implicit cpt: CanParse[T, A], cpa: CanParse[A, A]): ValidationNel[ParseError[A], Parsed[T]] =
    atRoot.flatMap(cpt.as(_))

  def sequenceOption[B](implicit ev: A <:< Option[B]): Option[Parsed[B]] =
    ev(value).map(next0(root, ops, _))
}

object Parsed {
  type Aux[A, Orig0] = Parsed[A] { type Orig = Orig0 }

  def apply[A](a: A, init: Op.Init.type, root: Vector[String] = Vector()): Aux[A, A] =
    new Parsed[A](root, Vector(init), a) {
      type Orig = A
      val orig = a
    }

  private [typify] def init[A](a: A, root: Vector[String] = Vector()): Aux[A, A] = apply(a, Op.Init, root)
}

@implicitNotFound(msg = "Cannot find CanParse from ${P} to ${T}")
trait CanParse[T, P] {
  def parse(k: String, p: Parsed[P]): ValidationNel[ParseError[P], Parsed[T]]
  def as(p: Parsed[P]): ValidationNel[ParseError[P], Parsed[T]]
}


object Typify {
  type E2L[L, P] = ParseError[P] => L
  type PV[P, L, A] = Parsed[P] => ValidationNel[L, A]
  type KPV[P, L, A] = String => PV[P, L, A]

  def validate[L, P, A, B](v: (String, Parsed[A], Parsed[P]) => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    (k: String) => (p: Parsed[P]) =>
      p.as[A](k).leftMap(_.map(e2l)).flatMap(v(k, _, p))

  def validate[L, P, A, B](v: Parsed[A] => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    validate((_: String, p: Parsed[A], _: Parsed[P]) => v(p))

  def validate[L, P, A, B](v: (String, Parsed[A]) => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    validate((s: String, p: Parsed[A], _: Parsed[P]) => v(s, p))

  def optional[L, P, A, B](v: (String, Parsed[A], Parsed[P]) => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    (k: String) => (p: Parsed[P]) =>
      p.as[Option[A]](k).leftMap(_.map(e2l))
        .flatMap(_.sequenceOption.map(v(k, _, p)).sequence)

  def optional[L, P, A, B](v: Parsed[A] => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    optional((_: String, p: Parsed[A], _: Parsed[P]) => v(p))

  def optional[L, P, A, B](v: (String, Parsed[A]) => ValidationNel[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    optional((s: String, p: Parsed[A], _: Parsed[P]) => v(s, p))
}

class Typify[L, P] {
  type PV[A] = Typify.PV[P, L, A]
  type KPV[A] = Typify.KPV[P, L, A]

  lazy val pvHNil: PV[HNil] = (_: Parsed[P]) => HNil.successNel[L]

  object foldPV extends Poly2 {
    implicit def PV[O <: HList, A]: Case.Aux[PV[A], PV[O], PV[A :: O]] =
      at((a, acc) => (p: Parsed[P]) => (a(p) |@| acc(p))(_ :: _))

    implicit def labelledPV[O <: HList, K <: Symbol, A]: Case.Aux[FieldType[K, PV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => (p: Parsed[P]) => (a(p) |@| acc(p))((x, y) => field[K](x) :: y))

    implicit def labelledKPV[O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => (p: Parsed[P]) => (a(k.value.name)(p) |@| acc(p))((x, y) => field[K](x) :: y))

    implicit def nested[O <: HList, K <: Symbol, I <: HList, FR, IR <: HList](
      implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, FR],
      ev: FR <:< PV[IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PV[O], PV[FieldType[K, IR] :: O]] =
      at((in, acc) => (p: Parsed[P]) => (rf(in, pvHNil)(p.addRoot(k.value.name)) |@| acc(p))((x, y) => field[K](x) :: y))
  }

  object syntax {
    implicit class HLOps(p: Parsed[P]) {
      def parse[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R]
      ): ValidationNel[L, R] =
        rf(in, pvHNil)(p)

      def parseOption[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R],
        e2l: Typify.E2L[L, P],
        cpop: CanParse[Option[P], P]
      ): ValidationNel[L, Option[R]] =
        p.atRootO.leftMap(_.map(e2l)).flatMap(_.sequenceOption.traverse(_.parse(in)))
    }
  }
}
