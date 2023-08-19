package typify.record

import typify.tuple.{Case1, DepFn1}

trait MapValues[F, L] extends DepFn1[L]

object MapValues {
  type Aux[F, L, O] = MapValues[F, L] { type Out = O }

  inline def apply[F, L](using m: MapValues[F, L]): MapValues.Aux[F, L, m.Out] = m

  given emptyTupleMapValues[F, L <: EmptyTuple]: MapValues.Aux[F, L, EmptyTuple] =
    new MapValues[F, L] {
      type Out = EmptyTuple
      def apply(l: L) = EmptyTuple
    }

  given tupleNMapValues[F, K, V, O, T <: Tuple](
    using ch: Case1[F, V, O],
    mt: MapValues[F, T] { type Out <: Tuple },
  ): MapValues.Aux[F, (K ->> V) *: T, (K ->> O) *: mt.Out] =
    new MapValues[F, (K ->> V) *: T] {
      type Out = (K ->> O) *: mt.Out
      def apply(l: (K ->> V) *: T) = label[K](ch.run(l.head)) *: mt(l.tail)
    }
}
