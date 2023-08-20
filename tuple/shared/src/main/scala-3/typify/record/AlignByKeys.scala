package typify.record

import typify.tuple.DepFn1

/**
 * Type class reordering record `T` by the `Tuple` of keys `K`.
 */
trait AlignByKeys[T, K] extends DepFn1[T]

object AlignByKeys {
  type Aux[T, K, O] = AlignByKeys[T, K] { type Out = O }

  inline def apply[T, K](using a: AlignByKeys[T, K]): AlignByKeys.Aux[T, K, a.Out] = a

  given emptyTupleAlign: AlignByKeys.Aux[EmptyTuple, EmptyTuple, EmptyTuple] =
    new AlignByKeys[EmptyTuple, EmptyTuple] {
      type Out = EmptyTuple
      def apply(t: EmptyTuple): EmptyTuple = EmptyTuple
    }

  given tupleNAlign[T <: Tuple, KH, KT <: Tuple, V, R <: Tuple, TA <: Tuple](
    using rh: Remover.Aux[T, KH, (V, R)],
    at: AlignByKeys.Aux[R, KT, TA]
  ): AlignByKeys.Aux[T, KH *: KT, (KH ->> V) *: TA] =
    new AlignByKeys[T, KH *: KT] {
      type Out = (KH ->> V) *: TA
      def apply(t: T): (KH ->> V) *: TA = {
        val (v, r) = rh(t)
        label[KH](v) *: at(r)
      }
    }
}
