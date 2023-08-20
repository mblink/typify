package typify.record

import typify.tuple.{DepFn1, Lub}

/**
 * Type class supporting converting this record to a `Map` whose keys and values
 * are typed as the Lub of the keys and values of this record.
 */
trait ToMap[L] extends DepFn1[L] {
  type Key
  type Value
  final type Out = Map[Key, Value]
}

object ToMap {
  type Aux[L, K, V] = ToMap[L] {
    type Key = K
    type Value = V
  }

  inline def apply[L](using t: ToMap[L]): ToMap.Aux[L, t.Key, t.Value] = t

  given emptyTupleToMap[K, V, L <: EmptyTuple]: ToMap.Aux[L, K, V] =
    new ToMap[L] {
      type Key = K
      type Value = V
      def apply(l: L) = Map.empty
    }

  given emptyTupleToMapAnyNothing[L <: EmptyTuple]: ToMap.Aux[L, Any, Nothing] = emptyTupleToMap[Any, Nothing, L]

  given hsingleToMap[K, V](using k: ValueOf[K]): ToMap.Aux[(K ->> V) *: EmptyTuple, K, V] =
    new ToMap[(K ->> V) *: EmptyTuple] {
      type Key = K
      type Value = V
      def apply(l: (K ->> V) *: EmptyTuple) = Map(k.value -> (l.head: V))
    }

  given hlistToMap[HK, HV, TH, TT <: Tuple, TK, TV, K, V](
    using tailToMap: ToMap.Aux[TH *: TT, TK, TV],
    keyLub: Lub[HK, TK, K],
    valueLub: Lub[HV, TV, V],
    k: ValueOf[HK]
  ): ToMap.Aux[(HK ->> HV) *: TH *: TT, K, V] =
    new ToMap[(HK ->> HV) *: TH *: TT] {
      type Key = K
      type Value = V
      def apply(l: (HK ->> HV) *: TH *: TT) =
        tailToMap(l.tail).map { case (k, v) => keyLub.right(k) -> valueLub.right(v) } +
          (keyLub.left(k.value) -> valueLub.left(l.head: HV))
    }
}
