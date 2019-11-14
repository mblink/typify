package typify

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scalaz.{Monad, ReaderWriterStateT, ValidationNel}
import scalaz.Validation.FlatMap._
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.applicative._
import scalaz.syntax.bifunctor._
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

  def downField[A](a: A, k: String): ValidationNel[ParseError, (Vector[Op], A)] =
    (Vector(DownField(k)), a).successNel[ParseError]

  def downFieldError[A](ops: Vector[Op], k: String)(implicit ct: ClassTag[A]): ValidationNel[ParseError, (Vector[Op], A)] =
    ParseError(ops, DownField(k), s"Could not be parsed as $ct").failureNel[(Vector[Op], A)]

  def typedValue[A](a: A): ValidationNel[ParseError, (Vector[Op], A)] =
    (Vector(TypeValue(a)), a).successNel[ParseError]

  trait MkTVE[A] {
    def apply[B](ops: Vector[Op], b: B)(implicit ct: ClassTag[A]): ValidationNel[ParseError, (Vector[Op], A)] =
      ParseError(ops, TypeValue(b), s"Could not be interpreted as $ct").failureNel[(Vector[Op], A)]
  }

  def typedValueError[A]: MkTVE[A] = new MkTVE[A] {}
}

trait MkValidated[L] {
  def apply[A](a: A): Validated0[L, A] =
    ReaderWriterStateT((_: Vector[Op], _: Unit) => (Vector[Op](), a, ()).successNel[L])
}

case class ParseError(ops: Vector[Op], failedOp: Op, error: String)

trait ValidatedHelper {
  protected implicit def vmonad[L]: Monad[ValidationNel[L, ?]] = new Monad[ValidationNel[L, ?]] {
    def point[A0](a: => A0): ValidationNel[L, A0] = a.successNel[L]

    def bind[A0, B](fa: ValidationNel[L, A0])(f: A0 => ValidationNel[L, B]): ValidationNel[L, B] =
      fa.flatMap(f)
  }

  implicit class ValidatedOps[L, A](v: Validated0[L, A]) {
    def leftMap[M](f: L => M): Validated0[M, A] =
      ReaderWriterStateT((ops: Vector[Op], s: Unit) => v.run(ops, s).leftMap(_.map(f)))
  }
}

sealed abstract case class Parsed[A](val root: Vector[String], val ops: Vector[Op], val value: A) extends ValidatedHelper { self =>
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

  def as[T](key: String)(implicit cpt: CanParse[T, A], cpa: CanParse[A, A]): Validated[T] =
    root.foldLeft(value.point[Validated])((r, k) => r.flatMap(cpa.parse(k, _))).flatMap(cpt.parse(key, _))

  def to[T](implicit cpt: CanParse[T, A], cpa: CanParse[A, A]): Validated[T] =
    root.foldLeft(value.point[Validated])((r, k) => r.flatMap(cpa.parse(k, _))).flatMap(cpt.as(_))

  // def atRootO(implicit cpa: CanParse[Option[A], A], ev: self.type <:< Parsed.Aux[A, A]): ValidationNel[ParseError, Parsed.Aux[Option[A], A]] =
  //   root match {
  //     case h +: t => cpa.parse(h, self.value).flatMap { case (oa, os) =>
  //       oa.fold(ev(self).next0(t, ops ++ os.list.toVector, none[A]).successNel[ParseError])(
  //         a => ev(self).next0(t, ops ++ os.list.toVector, a).atRootO)
  //     }
  //     case Vector() => ev(self).next0(root, ops, some(value)).successNel[ParseError]
  //   }

  // def sequenceOption[B](implicit ev: A <:< Option[B]): Option[Parsed.Aux[B, Orig]] =
  //   ev(value).map(next0(root, ops, _))
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
  def parse(k: String, p: P): Validated[T]
  def as(p: P): Validated[T]
}


object Typify extends ValidatedHelper {
  def validate[L, P, A, B](v: (String, A, Parsed.Aux[P, P]) => Validated0[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    (k: String) => (p: Parsed.Aux[P, P]) =>
      p.as[A](k).leftMap(e2l(p, _)).flatMap(v(k, _, p))

  def validate[L, P, A, B](v: A => Validated0[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    validate((_: String, p: A, _: Parsed.Aux[P, P]) => v(p))

  def validate[L, P, A, B](v: (String, A) => Validated0[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    validate((s: String, p: A, _: Parsed.Aux[P, P]) => v(s, p))

  def optional[L, P, A, B](v: (String, A, Parsed.Aux[P, P]) => Validated0[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    (k: String) => (p: Parsed.Aux[P, P]) =>
      p.as[Option[A]](k).leftMap(e2l(p, _)).flatMap(_.traverse(v(k, _, p)))

  def optional[L, P, A, B](v: A => Validated0[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    optional((_: String, p: A, _: Parsed.Aux[P, P]) => v(p))

  def optional[L, P, A, B](v: (String, A) => Validated0[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    optional((s: String, p: A, _: Parsed.Aux[P, P]) => v(s, p))
}

class Typify[L, P] extends ValidatedHelper {
  type PV[A] = typify.PV[P, L, A]
  type KPV[A] = typify.KPV[P, L, A]

  lazy val pvHNil: PV[HNil] = (_: Parsed.Aux[P, P]) => Validated0(HNil)

  object foldPV extends Poly2 {
    implicit def PV[O <: HList, A]: Case.Aux[PV[A], PV[O], PV[A :: O]] =
      at((a, acc) => (p: Parsed.Aux[P, P]) => (a(p) |@| acc(p))(_ :: _))

    implicit def labelledPV[O <: HList, K <: Symbol, A]: Case.Aux[FieldType[K, PV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => (p: Parsed.Aux[P, P]) => (a(p) |@| acc(p))((x, y) => field[K](x) :: y))

    implicit def labelledKPV[O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[A]], PV[O], PV[FieldType[K, A] :: O]] =
      at((a, acc) => (p: Parsed.Aux[P, P]) => (a(k.value.name)(p) |@| acc(p))((x, y) => field[K](x) :: y))

    implicit def nested[O <: HList, K <: Symbol, I <: HList, FR, IR <: HList](
      implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, FR],
      ev: FR <:< PV[IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PV[O], PV[FieldType[K, IR] :: O]] =
      at((in, acc) => (p: Parsed.Aux[P, P]) => (rf(in, pvHNil)(p.addRoot(k.value.name)) |@| acc(p))((x, y) => field[K](x) :: y))
  }

  object syntax {
    implicit class HLOps(p: Parsed.Aux[P, P]) {
      def parse[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R]
      ): ValidationNel[L, (R, Vector[Op])] = ???
        // rf(in, pvHNil)(p)

      // def parseOption[I <: HList, A, R <: HList](in: I)(
      //   implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
      //   pvaEv: A <:< PV[R],
      //   e2l: Typify.E2L[L, P],
      //   cpop: CanParse[Option[P], P]
      // ): ValidationNel[L, (Option[R], Vector[Op])] =
      //   p.root.foldLeft(cpop.as(p.value).map(_.rightMap(_.list.toVector)))(
      //       (r, k) => Typify.chainV(r)(_.fold(Op.typedValue(none[P]).successNel[ParseError])(cpop.parse(k, _))))
      //     .leftMap(_.map(e2l(p, _)))
          // .flatMap(_.traverse())

        // p.root.foldLeft(cp.as(p.run).disjunction)(
        //   (r, k) => r.flatMap(_.map(cp.parse(k, _).disjunction)
        //                        .getOrElse(none[B].successNel[ParseError].disjunction)))
        //  .leftMap(_.map(e2l(p, _)))
        //  .flatMap(_.traverseU(x => new HLOps(rev(Parsed(x))).parse(in)).disjunction)
        //  .validation

        //   p.atRootO.leftMap(_.map(e2l(p, _))).flatMap(_.sequenceOption.traverse(_.parse(in)))
    }
  }
}
