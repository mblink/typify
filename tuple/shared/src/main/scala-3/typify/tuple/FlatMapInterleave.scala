package typify.tuple

/**
 * Type class supporting interleaving an element into each row of this `Tuple` of `Tuple`s.
 */
trait FlatMapInterleave[A, M] extends DepFn2[A, M]

object FlatMapInterleave {
  type Aux[A, M, O] = FlatMapInterleave[A, M] { type Out = O }

  inline def apply[A, M](using i: FlatMapInterleave[A, M]): FlatMapInterleave.Aux[A, M, i.Out] = i

  given emptyTupleFlatMapInterleave[A, M <: EmptyTuple]: FlatMapInterleave.Aux[A, M, EmptyTuple] =
    new FlatMapInterleave[A, M] {
      type Out = EmptyTuple
      def apply(a: A, m: M): Out = EmptyTuple
    }

  given tupleNFlatMapInterleave[A, H <: Tuple, TM <: Tuple, HO <: Tuple, TMO <: Tuple, PrependOut <: Tuple](
    using ih: Interleave.Aux[A, H, HO],
    it: FlatMapInterleave.Aux[A, TM, TMO],
    p: Prepend.Aux[HO, TMO, PrependOut]
  ): FlatMapInterleave.Aux[A, H *: TM, PrependOut] =
    new FlatMapInterleave[A, H *: TM] {
      type Out = PrependOut
      def apply(a: A, m: H *: TM): Out =
        p(ih(a, m.head), it(a, m.tail))
    }
}
