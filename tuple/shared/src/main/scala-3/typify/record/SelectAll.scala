package typify.record

import typify.tuple.*

trait SelectAll[L, K] extends DepFn1[L]

object SelectAll {
  type Aux[L, K, O] = SelectAll[L, K] { type Out = O }

  inline def apply[L, K](using s: SelectAll[L, K]): SelectAll.Aux[L, K, s.Out] = s

  given emptyTupleSelectAll[L]: SelectAll.Aux[L, EmptyTuple, EmptyTuple] =
    new SelectAll[L, EmptyTuple] {
      type Out = EmptyTuple
      def apply(l: L): Out = EmptyTuple
    }

  given tupleNSelectAll[L <: Tuple, KH, KT <: Tuple](
    using sh: Selector[L, KH],
    st: SelectAll[L, KT] { type Out <: Tuple },
  ): SelectAll.Aux[L, KH *: KT, sh.Out *: st.Out] =
    new SelectAll[L, KH *: KT] {
      type Out = sh.Out *: st.Out
      def apply(l: L): Out = sh(l) *: st(l)
    }
}
