package typify.tuple

/**
 * Type class supporting removal of a sublist from this `Tuple`. Available only if this `Tuple` contains a
 * sublist of type `SL`.
 *
 * The elements of `SL` do not have to be contiguous in this `Tuple`.
 */
trait RemoveAll[L, SL] extends DepFn1[L] {
  def reinsert(out: Out): L
}

object RemoveAll {
  type Aux[L, SL, O] = RemoveAll[L, SL] { type Out = O }

  inline def apply[L, SL](using r: RemoveAll[L, SL]): RemoveAll.Aux[L, SL, r.Out] = r

  given emptyTupleRemoveAll[L]: RemoveAll.Aux[L, EmptyTuple, (EmptyTuple, L)] =
    new RemoveAll[L, EmptyTuple] {
      type Out = (EmptyTuple, L)
      def apply(l : L): Out = (EmptyTuple, l)
      def reinsert(out: (EmptyTuple, L)): L = out._2
    }

  given tupleNRemoveAll[L <: Tuple, E, RemE <: Tuple, Rem <: Tuple, SLT <: Tuple](
    using rt: Remove.Aux[L, E, (E, RemE)],
    st: RemoveAll.Aux[RemE, SLT, (SLT, Rem)],
  ): RemoveAll.Aux[L, E *: SLT, (E *: SLT, Rem)] =
    new RemoveAll[L, E *: SLT] {
      type Out = (E *: SLT, Rem)
      def apply(l : L): Out = {
        val (e, rem) = rt(l)
        val (sl, left) = st(rem)
        (e *: sl, left)
      }

      def reinsert(out: (E *: SLT, Rem)): L =
        rt.reinsert((out._1.head, st.reinsert((out._1.tail, out._2))))
    }
}
