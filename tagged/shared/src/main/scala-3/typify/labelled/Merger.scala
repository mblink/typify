package typify.labelled

import typify.tuple.DepFn2

trait Merger[L <: Tuple, M <: Tuple] extends DepFn2[L, M]

object Merger {
  type Aux[L <: Tuple, M <: Tuple, Out0] = Merger[L, M] { type Out = Out0 }

  inline def apply[L <: Tuple, M <: Tuple](using m: Merger[L, M]): Aux[L, M, m.Out] = m

  given emptyTupleMergerL[L <: NonEmptyTuple]: Aux[L, EmptyTuple, L] =
    new Merger[L, EmptyTuple] {
      type Out = L
      def apply(l: L, m: EmptyTuple): Out = l
    }

  given emptyTupleMergerR[M <: NonEmptyTuple]: Aux[EmptyTuple, M, M] =
    new Merger[EmptyTuple, M] {
      type Out = M
      def apply(l: EmptyTuple, m: M): Out = m
    }

  given updateTupleMerger[K <: Singleton, V, L <: Tuple, M <: Tuple, U <: Tuple](
    using u: Updater.Aux[L, K ->> V, U],
    mu: Merger[U, M],
  ): Aux[L, (K ->> V) *: M, mu.Out] =
    new Merger[L, (K ->> V) *: M] {
      type Out = mu.Out
      def apply(l: L, m: (K ->> V) *: M): Out =
        mu(u(l, m.head), m.tail)
    }
}
