package typify.tuple

import compiletime.ops.int.%

trait RotateLeft[L, N] extends DepFn1[L]

object RotateLeft {
  type Aux[L, N, O] = RotateLeft[L, N] { type Out = O }

  inline def apply[L, N](using r: RotateLeft[L, N]): RotateLeft.Aux[L, N, r.Out] = r

  given tupleRotateLeft[L <: Tuple, N <: Int](using nv: ValueOf[N], sizev: ValueOf[Tuple.Size[L]])
    : RotateLeft.Aux[L, N, Tuple.Concat[Tuple.Drop[L, N % Tuple.Size[L]], Tuple.Take[L, N % Tuple.Size[L]]]] =
    new RotateLeft[L, N] {
      type Out = Tuple.Concat[Tuple.Drop[L, N % Tuple.Size[L]], Tuple.Take[L, N % Tuple.Size[L]]]
      private lazy val n = nv.value
      private lazy val size = sizev.value
      private lazy val rem = n % size
      def apply(l: L): Out = {
        val a = l.toArray
        Tuple.fromArray(a.drop(rem) ++ a.take(rem)).asInstanceOf[Out]
      }
    }
}
