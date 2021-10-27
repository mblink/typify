import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.validated._
import scala.annotation.tailrec
import shapeless.{::, HList, HNil, Poly2, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist.RightFolder

package object typify extends typify.StringOps {
  type PV[P, L, A] = Cursor[P] => ValidatedNel[L, A]
  type KPV[P, L, A] = String => PV[P, L, A]

  type E2L[L, P] = ParseError[P] => L

  private[typify] def ap[A, B](a: A)(f: A => B): B = f(a)

  def parseList[A, L, O](cursor: Cursor[A])(
    fail: Cursor.Failed[A] => ValidatedNel[L, List[O]],
    proc: Cursor[A] => ValidatedNel[L, O]
  ): ValidatedNel[L, List[O]] = {
    lazy val empty = List[O]().validNel[L]

    cursor.downArray match {
      case f @ Cursor.Failed(_, _) => f.history.failedOp match {
        case Some(CursorOp.DownArray(true)) => empty
        case _ => fail(f)
      }
      case a =>
        @tailrec def go(c: Cursor[A], res: ValidatedNel[L, List[O]]): ValidatedNel[L, List[O]] =
          c match {
            case Cursor.Failed(_, _) => res
            case _ => go(c.right, (res, proc(c)).mapN(_ :+ _))
          }

        go(a, List[O]().validNel[L])
    }
  }

  type PVFolder[P, L, I <: HList, O <: HList] = RightFolder[I, PV[P, L, HNil], foldPV.type] { type Out <: PV[P, L, O] }

  def pvHNil[P, L]: PV[P, L, HNil] = (_: Cursor[P]) => HNil.validNel[L]

  object foldPV extends Poly2 {
    private def runA[P, L, A, B, O <: HList](a: PV[P, L, A], acc: PV[P, L, O])(f: A => B): PV[P, L, B :: O] =
      (c: Cursor[P]) => (a(c), acc(c)).mapN((x, y) => f(x) :: y)

    implicit def PV[P, L, O <: HList, A]: Case.Aux[PV[P, L, A], PV[P, L, O], PV[P, L, A :: O]] =
      at(runA(_, _)(identity[A] _))

    implicit def labelledPV[P, L, O <: HList, K, A]: Case.Aux[FieldType[K, PV[P, L, A]], PV[P, L, O], PV[P, L, FieldType[K, A] :: O]] =
      at((a, acc) => runA(a, acc)(field[K](_)))

    implicit def labelledKPVSym[P, L, O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[P, L, A]], PV[P, L, O], PV[P, L, FieldType[K, A] :: O]] =
      at((a, acc) => runA(a(k.value.name), acc)(field[K](_)))

    implicit def labelledKPVStr[P, L, O <: HList, K <: String, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[P, L, A]], PV[P, L, O], PV[P, L, FieldType[K, A] :: O]] =
      at((a, acc) => runA(a(k.value), acc)(field[K](_)))

    implicit def listOfPVSym[P, L, O <: HList, K <: Symbol, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, listOf[PV[P, L, A]]], PV[P, L, O], PV[P, L, FieldType[K, List[A]] :: O]] =
      at((in, acc) => (c: Cursor[P]) =>
        (typify.parseList(c.downField(k.value.name))(
          f => e2l(ParseError(f, "Could not be interpreted as List")).invalidNel[List[A]],
          in.run),
        acc(c)).mapN((x, y) => field[K](x) :: y))

    implicit def listOfPVStr[P, L, O <: HList, K <: String, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, listOf[PV[P, L, A]]], PV[P, L, O], PV[P, L, FieldType[K, List[A]] :: O]] =
      at((in, acc) => (c: Cursor[P]) =>
        (typify.parseList(c.downField(k.value))(
          f => e2l(ParseError(f, "Could not be interpreted as List")).invalidNel[List[A]],
          in.run),
        acc(c)).mapN((x, y) => field[K](x) :: y))

    implicit def listOfHList[P, L, O <: HList, K, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      lpv: Case.Aux[FieldType[K, listOf[PV[P, L, IR]]], PV[P, L, O], PV[P, L, FieldType[K, List[IR]] :: O]]
    ): Case.Aux[FieldType[K, listOf[I]], PV[P, L, O], PV[P, L, FieldType[K, List[IR]] :: O]] =
      at((in, acc) => lpv(field[K](listOf(rf(in.run, pvHNil)(_))), acc))

    implicit def nestedSym[P, L, O <: HList, K <: Symbol, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PV[P, L, O], PV[P, L, FieldType[K, IR] :: O]] =
      at((in, acc) => (c: Cursor[P]) =>
        (rf(in, pvHNil)(c.downField(k.value.name)), acc(c)).mapN((x, y) => field[K](x) :: y))

    implicit def nestedStr[P, L, O <: HList, K <: String, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PV[P, L, O], PV[P, L, FieldType[K, IR] :: O]] =
      at((in, acc) => (c: Cursor[P]) =>
        (rf(in, pvHNil)(c.downField(k.value)), acc(c)).mapN((x, y) => field[K](x) :: y))
  }
}
