package typify.record

import typify.tuple.DepFn1

/**
 * Type class supporting removal and re-insertion of a `Tuple` of elements (possibly unlabelled).
 */
trait RemoveAll[L, A] extends DepFn1[L] with Serializable {
  def reinsert(out: Out): L
}

object RemoveAll {
  type Aux[L, A, O] = RemoveAll[L, A] { type Out = O }

  inline def apply[L, A](using r: RemoveAll[L, A]): RemoveAll.Aux[L, A, r.Out] = r

  given emptyTupleRemoveAll[L]: RemoveAll.Aux[L, EmptyTuple, (EmptyTuple, L)] =
    new RemoveAll[L, EmptyTuple] {
      type Out = (EmptyTuple, L)
      def apply(l: L): Out = (EmptyTuple, l)
      def reinsert(out: Out): L = out._2
    }

  given tupleNRemoveAll[L <: Tuple, H, T <: Tuple, OutT <: Tuple, RemovedH, RemainderH <: Tuple, RemovedT <: Tuple, RemainderT <: Tuple](
    using rt: RemoveAll.Aux[L, T, (RemovedT, RemainderT)],
    rh: Remove.Aux[RemainderT, H, (RemovedH, RemainderH)],
  ): RemoveAll.Aux[L, H *: T, (RemovedH *: RemovedT, RemainderH)] =
      new RemoveAll[L, H *: T] {
        type Out = (RemovedH *: RemovedT, RemainderH)
        def apply(l: L): Out = {
          val (removedT, remainderT) = rt(l)
          val (removedH, remainderH) = rh(remainderT)
          (removedH *: removedT, remainderH)
        }
        def reinsert(out: Out): L = rt.reinsert((out._1.tail, rh.reinsert((out._1.head, out._2))))
      }
}
