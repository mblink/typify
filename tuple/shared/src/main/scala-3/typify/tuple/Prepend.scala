package typify.tuple

/**
 * Type class supporting prepending `L` to `R`.
 */
trait Prepend[L, R] extends DepFn2[L, R]

object Prepend {
  type Aux[L, R, O] = Prepend[L, R] { type Out = O }

  inline def apply[L, R](using p: Prepend[L, R]): Prepend.Aux[L, R, p.Out] = p

  given tuplePrepend[L <: Tuple, R <: Tuple]: Prepend.Aux[L, R, Tuple.Concat[L, R]] =
    new Prepend[L, R] {
      type Out = Tuple.Concat[L, R]
      def apply(l: L, r: R): Out = l ++ r
    }
}
