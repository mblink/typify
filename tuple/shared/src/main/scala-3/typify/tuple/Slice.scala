package typify.tuple

import compiletime.ops.int.{-, >=}

trait Slice[N, U, L] extends DepFn1[L]

object Slice {
  type Aux[N, U, L, O] = Slice[N, U, L] { type Out = O }

  inline def apply[N, U, L](using s: Slice[N, U, L]): Slice.Aux[N, U, L, s.Out] = s

  given tupleSlice[N <: Int, U <: Int, L <: Tuple](
    using sizeEv: (Tuple.Size[L] >= U) =:= true,
    rangeEv: (U >= N) =:= true,
    nv: ValueOf[N],
    uv: ValueOf[U]
  ): Slice.Aux[N, U, L, Tuple.Take[Tuple.Drop[L, N], U - N]] =
    new Slice[N, U, L] {
      type Out = Tuple.Take[Tuple.Drop[L, N], U - N]
      private lazy val n = nv.value
      private lazy val u = uv.value
      def apply(l: L): Out = {
        val a = l.toArray
        Tuple.fromArray(a.drop(n).take(u - n)).asInstanceOf[Out]
      }
    }
}
