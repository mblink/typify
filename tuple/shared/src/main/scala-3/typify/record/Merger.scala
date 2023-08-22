package typify.record

import typify.tuple.DepFn2

/**
 * Type class support record merging.
 */
trait Merger[L, M] extends DepFn2[L, M] with Serializable

object Merger {
  type Aux[L, M, O] = Merger[L, M] { type Out = O }

  inline def apply[L, M](using m: Merger[L, M]): Merger.Aux[L, M, m.Out] = m

  given emptyTupleMergerL[L]: Merger.Aux[L, EmptyTuple, L] =
    new Merger[L, EmptyTuple] {
      type Out = L
      def apply(l: L, m: EmptyTuple): Out = l
    }

  given emptyTupleMergerR[M]: Merger.Aux[EmptyTuple, M, M] =
    new Merger[EmptyTuple, M] {
      type Out = M
      def apply(l: EmptyTuple, m: M): Out = m
    }

  given updateTupleMerger[K <: Singleton, V, L, M <: Tuple, U](
    using u: Updater.Aux[L, K ->> V, U],
    mu: Merger[U, M],
  ): Merger.Aux[L, (K ->> V) *: M, mu.Out] =
    new Merger[L, (K ->> V) *: M] {
      type Out = mu.Out
      def apply(l: L, m: (K ->> V) *: M): Out =
        mu(u(l, m.head), m.tail)
    }
}
