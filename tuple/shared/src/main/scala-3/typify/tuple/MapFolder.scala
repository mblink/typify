package typify.tuple

/**
 * Type class supporting mapping a polymorphic function over this `Tuple` and then folding the result using a
 * monomorphic function value.
 */
trait MapFolder[L <: Tuple, R, F] {
  def apply(l: L, in: R, op: (R, R) => R): R
}

object MapFolder {
  inline def apply[L <: Tuple, R, F](using f: MapFolder[L, R, F]): MapFolder[L, R, F] = f

  given mapFolderEmptyTuple[R, F]: MapFolder[EmptyTuple, R, F] =
    new MapFolder[EmptyTuple, R, F] {
      def apply(l: EmptyTuple, in: R, op: (R, R) => R): R = in
    }

  given mapFolderTupleCons[H, T <: Tuple, R, F <: Poly](
    using hc: Case1.Aux[F, H, R],
    tf: MapFolder[T, R, F],
  ): MapFolder[H *: T, R, F] =
    new MapFolder[H *: T, R, F] {
      def apply(l: H *: T, in: R, op: (R, R) => R): R = op(hc(l.head), tf(l.tail, in, op))
    }
}
