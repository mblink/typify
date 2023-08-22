package typify.tuple

import compiletime.ops.int.>=

/**
 * Type class supporting retrieval of the first `N` elements of this `Tuple`. Available only if this `Tuple` has at
 * least `N` elements.
 */
trait Take[T, N] extends DepFn1[T]

object Take {
  type Aux[T, N, O] = Take[T, N] { type Out = O }

  inline def apply[T, N](using t: Take[T, N]): Take.Aux[T, N, t.Out] = t

  given takeTuple[T <: Tuple, N <: Int](
    using ev: (Tuple.Size[T] >= N) =:= true,
    n: ValueOf[N],
  ): Take.Aux[T, N, Tuple.Take[T, N]] =
    new Take[T, N] {
      type Out = Tuple.Take[T, N]
      def apply(t: T): Out = t.take(n.value).asInstanceOf[Out]
    }
}
