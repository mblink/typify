package typify.tuple

import compiletime.ops.int.>=

/**
 * Type class supporting removal of the first `N` elements of this `Tuple`. Available only if this `Tuple` has at least `N` elements.
 */
trait Drop[T, N] extends DepFn1[T]

object Drop {
  type Aux[T, N, O] = Drop[T, N] { type Out = O }

  inline def apply[T, N](using d: Drop[T, N]): Drop.Aux[T, N, d.Out] = d

  given dropTuple[T <: Tuple, N <: Int](
    using ev: (Tuple.Size[T] >= N) =:= true,
    n: ValueOf[N],
  ): Drop.Aux[T, N, Tuple.Drop[T, N]] =
    new Drop[T, N] {
      type Out = Tuple.Drop[T, N]
      def apply(t: T): Out = t.drop(n.value).asInstanceOf[Out]
    }
}
