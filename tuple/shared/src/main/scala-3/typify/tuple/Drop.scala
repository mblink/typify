package typify.tuple

import compiletime.ops.int.>=

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
      def apply(t: T): Out = Tuple.fromArray(t.toArray.drop(n.value)).asInstanceOf[Out]
    }
}
