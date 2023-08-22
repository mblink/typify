package typify.tuple

/**
 * Type class witnessing that a `Tuple` can be collected with a `Poly` of type `F` to produce a `Tuple`
 */
trait Collect[I, F] extends DepFn1[I] with Serializable

sealed trait CollectLP {
  final type Aux[L, F, O] = Collect[L, F] { type Out = O }

  final given tupleNNoPolyCase[LH, LT <: Tuple, P <: Poly, CollectOut <: Tuple](
    using ct: Collect.Aux[LT, P, CollectOut],
  ): Collect.Aux[LH *: LT, P, CollectOut] =
    new Collect[LH *: LT, P] {
      type Out = CollectOut
      def apply(l: LH *: LT): Out = ct(l.tail)
    }
}

object Collect extends CollectLP {
  inline def apply[L, F](using c: Collect[L, F]): Collect.Aux[L, F, c.Out] = c

  given emptyTupleCollect[P <: Poly]: Collect.Aux[EmptyTuple, P, EmptyTuple] =
    new Collect[EmptyTuple, P] {
      type Out = EmptyTuple
      def apply(l: EmptyTuple): Out = EmptyTuple
    }

  given tupleNCollect[LH, LT <: Tuple, P <: Poly, CollectOut <: Tuple, ClrResult](
    using ct: Collect.Aux[LT, P, CollectOut],
    ch: Case1.Aux[P, LH, ClrResult],
  ): Collect.Aux[LH *: LT, P, ClrResult *: CollectOut] =
    new Collect[LH *: LT, P] {
      type Out = ClrResult *: CollectOut
      def apply(l: LH *: LT): Out = ch(l.head) *: ct(l.tail)
    }
}
