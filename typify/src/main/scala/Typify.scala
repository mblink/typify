package typify

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scalaz.{/*Applicative,*/ ValidationNel}
// import scalaz.Validation.FlatMap._
import scalaz.std.option._
// import scalaz.std.tuple._
import scalaz.syntax.applicative._
// import scalaz.syntax.bifunctor._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import shapeless.{::, HList, HNil, Poly2, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist.RightFolder

// case class Validated[L, A](run: Vector[Op] => ValidationNel[L, (Vector[Op], A)]) {
//   def map[B](f: A => B): Validated[L, B] =
//     Validated(run(_).map(_.rightMap(f)))

//   private [typify] def flatMap[B](f: A => Validated[L, B]): Validated[L, B] =
//     Validated(ops => run(ops).flatMap { case (ops2, a) => f(a).run(ops ++ ops2) })
// }

object Validated2 {
  def debug(msg: String, vars: Any*): Unit =
    println(s"*************************************\n$msg\n\n${vars.mkString("\n\n")}\n\n${""/*(new Throwable).getStackTrace.mkString("\n")*/}\n*************************************")

  // implicit def applicative[L]: Applicative[Validated[L, ?]] = new Applicative[Validated[L, ?]] {
  //   def point[A](a: => A): Validated[L, A] =
  //     Validated(_ => (Vector[Op](), a).successNel[L])

  //   def ap[A, B](fa: => Validated[L, A])(fab: => Validated[L, A => B]): Validated[L, B] =
  //     Validated(ops => fa.run(ops).flatMap { case (os1, a) =>
  //       fab.run(ops ++ os1).map { case (os2, f) => (os1 ++ os2, f(a)) } })

  //     // Validated(ops => (fa.run(ops) |@| fab.run(ops)) { case ((os1, a), (os2, f)) => (os1 ++ os2, f(a)) })
  // }

  object syntax {
    implicit class ValidatedOps[L, A](val v: Validated[L, A]) extends AnyVal {
      def leftMapNel[M](f: L => M): Validated[M, A] = Validated(v.run(_).leftMap(_.map(f)))
    }
  }
}

sealed trait Op
object Op {
  case class ArrayIndex(index: Int) extends Op
  case class DownField(key: String) extends Op
  case class TypeValue[A](value: A) extends Op

  def downField[A](ops: Vector[Op], a: A, k: String): ValidationNel[ParseError, (Vector[Op], A)] =
    (ops :+ DownField(k), a).successNel[ParseError]

  def downFieldError[A](ops: Vector[Op], k: String)(implicit ct: ClassTag[A]): ValidationNel[ParseError, (Vector[Op], A)] =
    ParseError(ops, DownField(k), s"Could not be parsed as $ct").failureNel[(Vector[Op], A)]

  def typeValue[A](ops: Vector[Op], a: A): ValidationNel[ParseError, (Vector[Op], A)] =
    (ops :+ TypeValue(a), a).successNel[ParseError]

  trait MkTVE[A] {
    def apply[B](ops: Vector[Op], b: B)(implicit ct: ClassTag[A]): ValidationNel[ParseError, (Vector[Op], A)] =
      ParseError(ops, TypeValue(b), s"Could not be interpreted as $ct").failureNel[(Vector[Op], A)]
  }

  def typeValueError[A]: MkTVE[A] = new MkTVE[A] {}
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
  import Validated2.syntax._

  def validate[L, P, A, B](v: (String, A, Parsed[P]) => Validated[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    (k: String) => (p: Parsed[P]) =>
      p.as[A](k).leftMapNel(e2l(p, _)).flatMap(v(k, _, p))

  def validate[L, P, A, B](v: A => Validated[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    validate((_: String, p: A, _: Parsed[P]) => v(p))

  def validate[L, P, A, B](v: (String, A) => Validated[L, B])(
    implicit e2l: E2L[L, P], cpa: CanParse[A, P], cpp: CanParse[P, P]
  ): KPV[P, L, B] =
    validate((s: String, p: A, _: Parsed[P]) => v(s, p))

  def optional[L, P, A, B](v: (String, A, Parsed[P]) => Validated[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    (k: String) => (p: Parsed[P]) =>
      p.as[Option[A]](k).leftMapNel(e2l(p, _)).flatMap(_.traverse(v(k, _, p)))

  def optional[L, P, A, B](v: A => Validated[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    optional((_: String, p: A, _: Parsed[P]) => v(p))

  def optional[L, P, A, B](v: (String, A) => Validated[L, B])(
    implicit e2l: E2L[L, P], cp: CanParse[Option[A], P], cpp: CanParse[P, P]
  ): KPV[P, L, Option[B]] =
    optional((s: String, p: A, _: Parsed[P]) => v(s, p))
}

class Typify[L, P] {
  type PV[A] = typify.PV[P, L, A]
  type KPV[A] = typify.KPV[P, L, A]

  lazy val pvHNil: PV[HNil] = (_: Parsed[P]) => (HNil: HNil).point[Validated[L, ?]]

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
      at((in, acc) => (p: Parsed[P]) =>
        (rf(in, pvHNil)(p.withRoot(p.root :+ k.value.name)) |@|
         acc(p))((x, y) => field[K](x) :: y))
  }

  object syntax {
    import Validated2.syntax._

    implicit class HLOps(p: Parsed[P]) {
      def parse[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R]
      ): ValidationNel[L, (Vector[Op], R)] =
        rf(in, pvHNil)(p).run(Vector())

      def parseOption[I <: HList, A, R <: HList](in: I)(
        implicit rf: RightFolder.Aux[I, PV[HNil], foldPV.type, A],
        pvaEv: A <:< PV[R],
        e2l: E2L[L, P],
        cpop: CanParse[Option[P], P]
      ): Validated[L, Option[R]] =
        p.root.foldLeft(cpop.as(p.run))(
            (r, k) => r.flatMap(_.fold(Validated(Op.downField(_, none[P], k)))(cpop.parse(k, _))))
          .leftMapNel(e2l(p, _))
          .flatMap(_.traverse(x => Validated(_ => Parsed(x).parse(in))))
    }
  }
}
