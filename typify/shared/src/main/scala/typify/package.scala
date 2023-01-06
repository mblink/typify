import cats.{Applicative, Apply, Functor, Id}
import cats.arrow.FunctionK
import cats.data.ValidatedNel
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.validated._
import scala.annotation.tailrec
import shapeless.{::, HList, HNil, Poly2, Witness}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist.RightFolder

package object typify extends typify.StringOps {
  type PVF[F[_]] = { type T[P, L, A] = Cursor[P] => F[ValidatedNel[L, A]] }
  type KPVF[F[_]] = { type T[P, L, A] = String => PVF[F]#T[P, L, A] }

  type PV[P, L, A] = PVF[Id]#T[P, L, A]
  type KPV[P, L, A] = KPVF[Id]#T[P, L, A]

  type E2L[L, P] = ParseError[P] => L

  private[typify] def ap[A, B](a: A)(f: A => B): B = f(a)

  private implicit val idNT: FunctionK[Id, Id] = FunctionK.id[Id]
  private implicit def idF[F[_]]: FunctionK[F, F] = FunctionK.id[F]
  private implicit def idToF[F[_]: Applicative] = new FunctionK[Id, F] {
    def apply[A](a: Id[A]): F[A] = a.pure[F]
  }

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

    private def runFa[F[_]: Functor, P, L, A, B, O <: HList](a: PVF[F]#T[P, L, A], acc: PV[P, L, O])(f: A => B): PVF[F]#T[P, L, B :: O] =
      (c: Cursor[P]) => a(c).map(va => (va, acc(c)).mapN((x, y) => f(x) :: y))

    private def runFo[F[_]: Functor, P, L, A, B, O <: HList](a: PV[P, L, A], acc: PVF[F]#T[P, L, O])(f: A => B): PVF[F]#T[P, L, B :: O] =
      (c: Cursor[P]) => acc(c).map(vacc => (a(c), vacc).mapN((x, y) => f(x) :: y))

    private def runFao[F[_]: Apply, P, L, A, B, O <: HList](a: PVF[F]#T[P, L, A], acc: PVF[F]#T[P, L, O])(f: A => B): PVF[F]#T[P, L, B :: O] =
      (c: Cursor[P]) => (a(c), acc(c)).mapN((_, _).mapN((x, y) => f(x) :: y))

    private def pvGen[F[_], G[_], H[_], P, L, O <: HList, A](
      mk: (PVF[F]#T[P, L, A], PVF[G]#T[P, L, O]) => Function1[A, A] => PVF[H]#T[P, L, A :: O]
    ): Case.Aux[PVF[F]#T[P, L, A], PVF[G]#T[P, L, O], PVF[H]#T[P, L, A :: O]] =
      at(mk(_, _)(identity[A] _))

    private def labelledPvGen[F[_], G[_], H[_], P, L, O <: HList, K, A](
      mk: (FieldType[K, PVF[F]#T[P, L, A]], PVF[G]#T[P, L, O]) => Function1[A, FieldType[K, A]] => PVF[H]#T[P, L, FieldType[K, A] :: O]
    ): Case.Aux[FieldType[K, PVF[F]#T[P, L, A]], PVF[G]#T[P, L, O], PVF[H]#T[P, L, FieldType[K, A] :: O]] =
      at(mk(_, _)(field[K](_)))

    private def labelledKPVSymGen[F[_], G[_], H[_], P, L, O <: HList, K <: Symbol, A](
      mk: (PVF[F]#T[P, L, A], PVF[G]#T[P, L, O]) => Function1[A, FieldType[K, A]] =>  PVF[H]#T[P, L, FieldType[K, A] :: O]
    )(
      implicit k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, KPVF[F]#T[P, L, A]], PVF[G]#T[P, L, O], PVF[H]#T[P, L, FieldType[K, A] :: O]] =
      at((a, acc) => mk(a(k.value.name), acc)(field[K](_)))

    private def labelledKPVStrGen[F[_], G[_], H[_], P, L, O <: HList, K <: String, A](
      mk: (PVF[F]#T[P, L, A], PVF[G]#T[P, L, O]) => Function1[A, FieldType[K, A]] => PVF[H]#T[P, L, FieldType[K, A] :: O]
    )(
      implicit k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, KPVF[F]#T[P, L, A]], PVF[G]#T[P, L, O], PVF[H]#T[P, L, FieldType[K, A] :: O]] =
      at((a, acc) => mk(a(k.value), acc)(field[K](_)))

    private def listOfPVSymGen[F[_], G[_], H[_]: Apply, P, L, O <: HList, K <: Symbol, A](implicit
      liftF: FunctionK[F, H],
      liftG: FunctionK[G, H],
      k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, F[listOf[PV[P, L, A]]]], PVF[G]#T[P, L, O], PVF[H]#T[P, L, FieldType[K, List[A]] :: O]] =
      at((in: FieldType[K, F[listOf[PV[P, L, A]]]], acc: PVF[G]#T[P, L, O]) =>
        (c: Cursor[P]) => (liftF(in), liftG(acc(c))).mapN((la, acc) =>
        (typify.parseList(c.downField(k.value.name))(
          f => e2l(ParseError(f, "Could not be interpreted as List")).invalidNel[List[A]],
          la.run),
        acc).mapN((x, y) => field[K](x) :: y)))

    private def listOfPVStrGen[F[_], G[_], H[_]: Apply, P, L, O <: HList, K <: String, A](implicit
      liftF: FunctionK[F, H],
      liftG: FunctionK[G, H],
      k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, F[listOf[PV[P, L, A]]]], PVF[G]#T[P, L, O], PVF[H]#T[P, L, FieldType[K, List[A]] :: O]] =
      at((in, acc) => (c: Cursor[P]) => (liftF(in), liftG(acc(c))).mapN((la, vacc) =>
        (typify.parseList(c.downField(k.value))(
          f => e2l(ParseError(f, "Could not be interpreted as List")).invalidNel[List[A]],
          la.run),
        vacc).mapN((x, y) => field[K](x) :: y)))

    implicit def listOfHListGen[F[_]: Functor, G[_], H[_], P, L, O <: HList, K, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      lpv: Case.Aux[FieldType[K, listOf[PVF[F]#T[P, L, IR]]], PVF[G]#T[P, L, O], PVF[H]#T[P, L, FieldType[K, List[IR]] :: O]]
    ): Case.Aux[FieldType[K, F[listOf[I]]], PVF[G]#T[P, L, O], PVF[H]#T[P, L, FieldType[K, List[IR]] :: O]] =
      at((in, acc) => lpv(field[K](listOf((c: Cursor[P]) => (in: F[listOf[I]]).map(i => rf(i.run, pvHNil)(c)))), acc))

    private def nestedSymGen[F[_], G[_], H[_]: Apply, P, L, O <: HList, K <: Symbol, I <: HList, IR <: HList](implicit
      liftF: FunctionK[F, H],
      liftG: FunctionK[G, H],
      rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[F[FieldType[K, I]], PVF[G]#T[P, L, O], PVF[H]#T[P, L, FieldType[K, IR] :: O]] =
      at((in, acc) => (c: Cursor[P]) => (liftF(in), liftG(acc(c))).mapN((i, vacc) =>
        (rf(i, pvHNil)(c.downField(k.value.name)), vacc).mapN((x, y) => field[K](x) :: y)))

    private def nestedStrGen[F[_], G[_], H[_]: Apply, P, L, O <: HList, K <: String, I <: HList, IR <: HList](implicit
      liftF: FunctionK[F, H],
      liftG: FunctionK[G, H],
      rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[F[FieldType[K, I]], PVF[G]#T[P, L, O], PVF[H]#T[P, L, FieldType[K, IR] :: O]] =
      at((in, acc) => (c: Cursor[P]) => (liftF(in), liftG(acc(c))).mapN((i, vacc) =>
        (rf(i, pvHNil)(c.downField(k.value)), vacc).mapN((x, y) => field[K](x) :: y)))

    implicit def PV[P, L, O <: HList, A]: Case.Aux[PV[P, L, A], PV[P, L, O], PV[P, L, A :: O]] =
      pvGen[Id, Id, Id, P, L, O, A](runA)

    implicit def PvFa[F[_]: Functor, P, L, O <: HList, A]: Case.Aux[PVF[F]#T[P, L, A], PV[P, L, O], PVF[F]#T[P, L, A :: O]] =
      pvGen[F, Id, F, P, L, O, A](runFa)

    implicit def PvFo[F[_]: Functor, P, L, O <: HList, A]: Case.Aux[PV[P, L, A], PVF[F]#T[P, L, O], PVF[F]#T[P, L, A :: O]] =
      pvGen[Id, F, F, P, L, O, A](runFo)

    implicit def PvFao[F[_]: Apply, P, L, O <: HList, A]: Case.Aux[PVF[F]#T[P, L, A], PVF[F]#T[P, L, O], PVF[F]#T[P, L, A :: O]] =
      pvGen[F, F, F, P, L, O, A](runFao)

    implicit def labelledPV[P, L, O <: HList, K, A]: Case.Aux[FieldType[K, PV[P, L, A]], PV[P, L, O], PV[P, L, FieldType[K, A] :: O]] =
      labelledPvGen[Id, Id, Id, P, L, O, K, A](runA)

    implicit def labelledPvFa[F[_]: Functor, P, L, O <: HList, K, A]: Case.Aux[FieldType[K, PVF[F]#T[P, L, A]], PV[P, L, O], PVF[F]#T[P, L, FieldType[K, A] :: O]] =
      labelledPvGen[F, Id, F, P, L, O, K, A](runFa)

    implicit def labelledPvFo[F[_]: Functor, P, L, O <: HList, K, A]: Case.Aux[FieldType[K, PV[P, L, A]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, A] :: O]] =
      labelledPvGen[Id, F, F, P, L, O, K, A](runFo)

    implicit def labelledPvFao[F[_]: Apply, P, L, O <: HList, K, A]: Case.Aux[FieldType[K, PVF[F]#T[P, L, A]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, A] :: O]] =
      labelledPvGen[F, F, F, P, L, O, K, A](runFao)

    implicit def labelledKPVSym[P, L, O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[P, L, A]], PV[P, L, O], PV[P, L, FieldType[K, A] :: O]] =
      labelledKPVSymGen[Id, Id, Id, P, L, O, K, A](runA)

    implicit def labelledKPVSymFa[F[_]: Functor, P, L, O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPVF[F]#T[P, L, A]], PV[P, L, O], PVF[F]#T[P, L, FieldType[K, A] :: O]] =
      labelledKPVSymGen[F, Id, F, P, L, O, K, A](runFa)

    implicit def labelledKPVSymFo[F[_]: Functor, P, L, O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[P, L, A]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, A] :: O]] =
      labelledKPVSymGen[Id, F, F, P, L, O, K, A](runFo)

    implicit def labelledKPVSymFao[F[_]: Apply, P, L, O <: HList, K <: Symbol, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPVF[F]#T[P, L, A]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, A] :: O]] =
      labelledKPVSymGen[F, F, F, P, L, O, K, A](runFao)

    implicit def labelledKPVStr[P, L, O <: HList, K <: String, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[P, L, A]], PV[P, L, O], PV[P, L, FieldType[K, A] :: O]] =
      labelledKPVStrGen[Id, Id, Id, P, L, O, K, A](runA)

    implicit def labelledKPVStrFa[F[_]: Functor, P, L, O <: HList, K <: String, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPVF[F]#T[P, L, A]], PV[P, L, O], PVF[F]#T[P, L, FieldType[K, A] :: O]] =
      labelledKPVStrGen[F, Id, F, P, L, O, K, A](runFa)

    implicit def labelledKPVStrFo[F[_]: Functor, P, L, O <: HList, K <: String, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPV[P, L, A]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, A] :: O]] =
      labelledKPVStrGen[Id, F, F, P, L, O, K, A](runFo)

    implicit def labelledKPVStrFao[F[_]: Apply, P, L, O <: HList, K <: String, A](implicit k: Witness.Aux[K]): Case.Aux[FieldType[K, KPVF[F]#T[P, L, A]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, A] :: O]] =
      labelledKPVStrGen[F, F, F, P, L, O, K, A](runFao)

    implicit def listOfPVSym[P, L, O <: HList, K <: Symbol, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, listOf[PV[P, L, A]]], PV[P, L, O], PV[P, L, FieldType[K, List[A]] :: O]] =
      listOfPVSymGen[Id, Id, Id, P, L, O, K, A]

    implicit def listOfPVSymFa[F[_]: Applicative, P, L, O <: HList, K <: Symbol, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, F[listOf[PV[P, L, A]]]], PV[P, L, O], PVF[F]#T[P, L, FieldType[K, List[A]] :: O]] =
      listOfPVSymGen[F, Id, F, P, L, O, K, A]

    implicit def listOfPVSymFo[F[_]: Applicative, P, L, O <: HList, K <: Symbol, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, listOf[PV[P, L, A]]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, List[A]] :: O]] =
      listOfPVSymGen[Id, F, F, P, L, O, K, A]

    implicit def listOfPVSymFao[F[_]: Apply, P, L, O <: HList, K <: Symbol, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, F[listOf[PV[P, L, A]]]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, List[A]] :: O]] =
      listOfPVSymGen[F, F, F, P, L, O, K, A]

    implicit def listOfPVStr[P, L, O <: HList, K <: String, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, listOf[PV[P, L, A]]], PV[P, L, O], PV[P, L, FieldType[K, List[A]] :: O]] =
      listOfPVStrGen[Id, Id, Id, P, L, O, K, A]

    implicit def listOfPVStrFa[F[_]: Applicative, P, L, O <: HList, K <: String, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, F[listOf[PV[P, L, A]]]], PV[P, L, O], PVF[F]#T[P, L, FieldType[K, List[A]] :: O]] =
      listOfPVStrGen[F, Id, F, P, L, O, K, A]

    implicit def listOfPVStrFo[F[_]: Applicative, P, L, O <: HList, K <: String, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, listOf[PV[P, L, A]]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, List[A]] :: O]] =
      listOfPVStrGen[Id, F, F, P, L, O, K, A]

    implicit def listOfPVStrFao[F[_]: Applicative, P, L, O <: HList, K <: String, A](
      implicit k: Witness.Aux[K],
      e2l: E2L[L, P]
    ): Case.Aux[FieldType[K, F[listOf[PV[P, L, A]]]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, List[A]] :: O]] =
      listOfPVStrGen[F, F, F, P, L, O, K, A]

    implicit def nestedSym[P, L, O <: HList, K <: Symbol, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PV[P, L, O], PV[P, L, FieldType[K, IR] :: O]] =
      nestedSymGen[Id, Id, Id, P, L, O, K, I, IR]

    implicit def nestedSymFa[F[_]: Applicative, P, L, O <: HList, K <: Symbol, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[F[FieldType[K, I]], PV[P, L, O], PVF[F]#T[P, L, FieldType[K, IR] :: O]] =
      nestedSymGen[F, Id, F, P, L, O, K, I, IR]

    implicit def nestedSymFo[F[_]: Applicative, P, L, O <: HList, K <: Symbol, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, IR] :: O]] =
      nestedSymGen[Id, F, F, P, L, O, K, I, IR]

    implicit def nestedSymFao[F[_]: Apply, P, L, O <: HList, K <: Symbol, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[F[FieldType[K, I]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, IR] :: O]] =
      nestedSymGen[F, F, F, P, L, O, K, I, IR]

    implicit def nestedStr[P, L, O <: HList, K <: String, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PV[P, L, O], PV[P, L, FieldType[K, IR] :: O]] =
      nestedStrGen[Id, Id, Id, P, L, O, K, I, IR]

    implicit def nestedStrFa[F[_]: Applicative, P, L, O <: HList, K <: String, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[F[FieldType[K, I]], PV[P, L, O], PVF[F]#T[P, L, FieldType[K, IR] :: O]] =
      nestedStrGen[F, Id, F, P, L, O, K, I, IR]

    implicit def nestedStrFo[F[_]: Applicative, P, L, O <: HList, K <: String, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[FieldType[K, I], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, IR] :: O]] =
      nestedStrGen[Id, F, F, P, L, O, K, I, IR]

    implicit def nestedStrFao[F[_]: Apply, P, L, O <: HList, K <: String, I <: HList, IR <: HList](
      implicit rf: PVFolder[P, L, I, IR],
      k: Witness.Aux[K]
    ): Case.Aux[F[FieldType[K, I]], PVF[F]#T[P, L, O], PVF[F]#T[P, L, FieldType[K, IR] :: O]] =
      nestedStrGen[F, F, F, P, L, O, K, I, IR]
  }
}
