package typify

import cats.syntax.apply._
import cats.syntax.validated._
import formless.hlist._
import formless.record._

sealed trait PVFolder[P, L, I <: HList, O <: HList]
extends (I => PV[P, L, HNil] => PV[P, L, O])

object PVFolder {
  sealed trait Case[I, O] extends (I => O)

  object Case {
    private def inst[I, O](f: I => O): Case[I, O] = new Case[I, O] {
      def apply(i: I): O = f(i)
    }

    implicit def PV[P, L, O <: HList, A]: Case[PV[P, L, A], PV[P, L, A]] =
      inst(identity)

    implicit def labelledPV[P, L, O <: HList, K, A]: Case[K ->> PV[P, L, A], PV[P, L, K ->> A]] =
      inst(_.andThen(_.map(label[K][A](_))))

    implicit def labelledKPVStr[P, L, O <: HList, K <: String, A](implicit k: ValueOf[K]): Case[K ->> KPV[P, L, A], PV[P, L, K ->> A]] =
      inst(a => a(k.value).andThen(_.map(label[K][A](_))))

    implicit def listOfPVStr[P, L, O <: HList, K <: String, A](
      implicit k: ValueOf[K],
      e2l: E2L[L, P]
    ): Case[K ->> listOf[PV[P, L, A]], PV[P, L, K ->> List[A]]] =
      inst(in => (c: Cursor[P]) => typify.parseList(c.downField(k.value))(
        f => e2l(ParseError(f, "Could not be interpreted as List")).invalidNel[List[A]],
        in.run
      ).map(label[K][List[A]](_)))

    implicit def listOfHList[P, L, O <: HList, K, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      lpv: Case[K ->> listOf[PV[P, L, IR]], PV[P, L, K ->> List[IR]]]
    ): Case[K ->> listOf[I], PV[P, L, K ->> List[IR]]] =
      inst { in =>
        val x = label[K](listOf(rf(in.run)(pvHNil)))
        lpv(x)
      }

    implicit def nestedStr[P, L, O <: HList, K <: String, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: ValueOf[K]
    ): Case[K ->> I, PV[P, L, K ->> IR]] =
      inst(in => (c: Cursor[P]) => rf(in)(pvHNil)(c.downField(k.value)).map(label[K][IR](_)))
  }

  private def inst[P, L, I <: HList, O <: HList](f: I => PV[P, L, HNil] => PV[P, L, O]): PVFolder[P, L, I, O] =
    new PVFolder[P, L, I, O] {
      def apply(i: I): PV[P, L, HNil] => PV[P, L, O] = f(i)
    }

  implicit def hnil[P, L]: PVFolder[P, L, HNil, HNil] =
    inst(_ => identity)

  implicit def hcons[P, L, HI, HO, TI <: HList, TO <: HList](
    implicit ch: Case[HI, PV[P, L, HO]],
    ft: PVFolder[P, L, TI, TO]
  ): PVFolder[P, L, HI :: TI, HO :: TO] =
    inst(t => pve => (c: Cursor[P]) => (ch(t.head)(c), ft(t.tail)(pve)(c)).mapN(_ :: _))
}
