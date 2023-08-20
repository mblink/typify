package typify.tuple

/**
 * Type class supporting adding an element to each possible position in this `Tuple`.
 */
trait Interleave[A, L] extends DepFn2[A, L]

object Interleave {
  type Aux[A, L, O] = Interleave[A, L] { type Out = O }

  inline def apply[A, L](using i: Interleave[A, L]): Interleave.Aux[A, L, i.Out] = i

  given emptyTupleInterleave[A, L <: EmptyTuple]: Interleave.Aux[A, L, (A *: EmptyTuple) *: EmptyTuple] =
    new Interleave[A, L] {
      type Out = (A *: EmptyTuple) *: EmptyTuple
      def apply(a: A, l: L): Out = (a *: EmptyTuple) *: EmptyTuple
    }

  given tupleNInterleave[A, H, T <: Tuple, LI <: Tuple, MapConsOut <: Tuple](
    using it: Interleave.Aux[A, T, LI],
    mh: MapCons.Aux[H, LI, MapConsOut],
  ): Interleave.Aux[A, H *: T, (A *: H *: T) *: MapConsOut] =
    new Interleave[A, H *: T] {
      type Out = (A *: H *: T) *: MapConsOut
      def apply(a: A, l: H *: T): Out = (a *: l) *: mh(l.head, it(a, l.tail))
    }
}
