package typify

import cats.syntax.apply._
import cats.syntax.validated._

sealed trait PVFolder[P, L, I <: Tuple, O <: Tuple]
extends (I => PV[P, L, EmptyTuple] => PV[P, L, O])

object PVFolder {
  sealed trait Case[I, Acc, Out] extends (I => Acc => Out)

  object Case {
    private def inst[I, Acc, Out](f: I => Acc => Out): Case[I, Acc, Out] = new Case[I, Acc, Out] {
      def apply(i: I): Acc => Out = f(i)
    }

    private def runA[P, L, A, B, O <: Tuple](a: PV[P, L, A])(acc: PV[P, L, O])(f: A => B): PV[P, L, B *: O] =
      (c: Cursor[P]) => (a(c), acc(c)).mapN((x, y) => f(x) *: y)

    implicit def PV[P, L, O <: Tuple, A]: Case[PV[P, L, A], PV[P, L, O], PV[P, L, A *: O]] =
      inst(a => acc => runA(a)(acc)(identity))

    implicit def labelledPV[P, L, O <: Tuple, K, A]: Case[Labelled[K, PV[P, L, A]], PV[P, L, O], PV[P, L, Labelled[K, A] *: O]] =
      inst(a => acc => runA(a.value)(acc)(Labelled[K][A](_)))

    implicit def labelledKPVStr[P, L, O <: Tuple, K <: String, A](implicit k: ValueOf[K]): Case[Labelled[K, KPV[P, L, A]], PV[P, L, O], PV[P, L, Labelled[K, A] *: O]] =
      inst(a => acc => runA(a.value(k.value))(acc)(Labelled[K][A](_)))

    implicit def listOfPVStr[P, L, O <: Tuple, K <: String, A](
      implicit k: ValueOf[K],
      e2l: E2L[L, P]
    ): Case[Labelled[K, listOf[PV[P, L, A]]], PV[P, L, O], PV[P, L, Labelled[K, List[A]] *: O]] =
      inst(in => acc => (c: Cursor[P]) => (
        typify.parseList(c.downField(k.value))(
          f => e2l(ParseError(f, "Could not be interpreted as List")).invalidNel[List[A]],
          in.value.run),
        acc(c)
      ).mapN((x, y) => Labelled[K](x) *: y))

    implicit def listOfTuple[P, L, O <: Tuple, K, I <: Tuple, IR <: Tuple](
      implicit rf: PVFolder[P, L, I, IR],
      lpv: Case[Labelled[K, listOf[PV[P, L, IR]]], PV[P, L, O], PV[P, L, Labelled[K, List[IR]] *: O]]
    ): Case[Labelled[K, listOf[I]], PV[P, L, O], PV[P, L, Labelled[K, List[IR]] *: O]] =
      inst { in =>
        val x = Labelled[K](listOf(rf(in.value.run)(pvEmptyTuple)))
        acc => lpv(x)(acc)
      }

    implicit def nestedStr[P, L, O <: Tuple, K <: String, I <: Tuple, IR <: Tuple](
      implicit rf: PVFolder[P, L, I, IR],
      k: ValueOf[K]
    ): Case[Labelled[K, I], PV[P, L, O], PV[P, L, Labelled[K, IR] *: O]] =
      inst(in => acc => (c: Cursor[P]) => (rf(in.value)(pvEmptyTuple)(c.downField(k.value)), acc(c)).mapN((x, y) => Labelled[K](x) *: y))
  }
}
