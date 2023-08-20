package typify.tuple

/**
 * Type class supporting the calculation of every permutation of this `Tuple`
 */
trait Permutations[L] extends DepFn1[L]

object Permutations {
  type Aux[L, O] = Permutations[L] { type Out = O }

  inline def apply[L](using p: Permutations[L]): Permutations.Aux[L, p.Out] = p

  given emptyTuplePermutations[L <: EmptyTuple]: Permutations.Aux[L, EmptyTuple *: EmptyTuple] =
    new Permutations[L] {
      type Out = EmptyTuple *: EmptyTuple
      def apply(l: L): Out = EmptyTuple *: EmptyTuple
    }

  given tupleNPermutations[H, T <: Tuple, TP <: Tuple, FlatmapInterleaveOut <: Tuple](
    using pt: Permutations.Aux[T, TP],
    ih: FlatMapInterleave.Aux[H, TP, FlatmapInterleaveOut],
  ): Permutations.Aux[H *: T, FlatmapInterleaveOut] =
    new Permutations[H *: T] {
      type Out = FlatmapInterleaveOut
      def apply(l: H *: T): Out = ih(l.head, pt(l.tail))
    }
}
