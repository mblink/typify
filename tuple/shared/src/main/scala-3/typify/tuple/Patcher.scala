package typify.tuple

import compiletime.ops.int.{+, >=}

/**
 * Type class supporting the patching of a `Tuple`.
 */
trait Patcher[N, M, L, In] extends DepFn2[L, In] with Serializable

object Patcher {
  type Aux[N, M, L, In, O] = Patcher[N, M, L, In] { type Out = O }

  inline def apply[N, M, L, In](using p: Patcher[N, M, L, In]): Patcher.Aux[N, M, L, In, p.Out] = p

  given tuplePatch[N <: Int, M <: Int, L <: Tuple, In <: Tuple](
    using ev: (Tuple.Size[L] >= N) =:= true,
    nv: ValueOf[N],
    mv: ValueOf[M],
  ): Patcher.Aux[N, M, L, In, Tuple.Concat[Tuple.Take[L, N], Tuple.Concat[In, Tuple.Drop[L, N + M]]]] =
    new Patcher[N, M, L, In] {
      type Out = Tuple.Concat[Tuple.Take[L, N], Tuple.Concat[In, Tuple.Drop[L, N + M]]]
      private lazy val n = nv.value
      private lazy val m = mv.value
      def apply(l: L, in: In): Out = {
        val a = l.toArray
        Tuple.fromArray(a.take(n) ++ (in.toArray ++ a.drop(n + m))).asInstanceOf[Out]
      }
    }
}
