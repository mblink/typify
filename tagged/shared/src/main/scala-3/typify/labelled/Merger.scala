package typify
package labelled

import scala.util.chaining.*

trait Merger[L <: Tuple, M <: Tuple] {
  type Out
  def apply(l: L, m: M): Out
}

trait MergerLP {
  type Aux[L <: Tuple, M <: Tuple, Out0] = Merger[L, M] { type Out = Out0 }

  given tupleMerger1[H, T <: Tuple, M <: Tuple](using mt: Merger[T, M] { type Out <: Tuple }): Aux[H *: T, M, H *: mt.Out] =
    new Merger[H *: T, M] {
      type Out = H *: mt.Out
      def apply(l: H *: T, m: M): Out = l.head *: mt(l.tail, m)
    }
}

object Merger extends MergerLP {
  inline def apply[L <: Tuple, M <: Tuple](using m: Merger[L, M]): Aux[L, M, m.Out] = m

  given emptyTupleMerger[M <: Tuple]: Aux[EmptyTuple, M, M] =
    new Merger[EmptyTuple, M] {
      type Out = M
      def apply(l: EmptyTuple, m: M): Out = m
    }

  given tupleMerger2[K, V, T <: Tuple, M <: Tuple, MT <: Tuple](
    using rm: Remover.Aux[M, K, (V, MT)],
    mt: Merger[T, MT] { type Out <: Tuple }
  ): Aux[(K ->> V) *: T, M, (K ->> V) *: mt.Out] =
    new Merger[(K ->> V) *: T, M] {
      type Out = (K ->> V) *: mt.Out
      def apply(l: (K ->> V) *: T, m: M): Out =
        rm(m).pipe { case (mv, mr) => label[K](mv) *: mt(l.tail, mr) }
    }
}
