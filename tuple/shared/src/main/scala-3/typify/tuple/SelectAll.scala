package typify.tuple

trait SelectAll[L, S] extends DepFn1[L] { type Out = S }

object SelectAll {
  inline def apply[L, S](using s: SelectAll[L, S]): SelectAll[L, S] = s

  given emptyTupleSelectAll[L]: SelectAll[L, EmptyTuple] =
    new SelectAll[L, EmptyTuple] {
      def apply(l: L): Out = EmptyTuple
    }

  given tupleNSelectAll[L <: Tuple, H, S <: Tuple](
    using sh: TupleSelector[L, H],
    st: SelectAll[L, S],
  ): SelectAll[L, H *: S] =
    new SelectAll[L, H *: S] {
      def apply(l: L): Out = sh(l) *: st(l)
    }
}
