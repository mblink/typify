package typify.tuple

import compiletime.ops.int.>

trait ReplaceAt[L, N, V] extends DepFn2[L, V]

object ReplaceAt {
  type Aux[L, N, V, O] = ReplaceAt[L, N, V] { type Out = O }

  inline def apply[L, N, V](using r: ReplaceAt[L, N, V]): ReplaceAt.Aux[L, N, V, r.Out] = r

  given replaceAtTuple[L <: Tuple, N <: Int, V](
    using ev: (Tuple.Size[L] > N) =:= true,
    n: ValueOf[N],
  ): ReplaceAt.Aux[L, N, V, (Tuple.Elem[L, N], ReplaceAtIndex[L, N, V])] =
    new ReplaceAt[L, N, V] {
      type Out = (Tuple.Elem[L, N], ReplaceAtIndex[L, N, V])
      def apply(l: L, v: V): Out = {
        val b = l.toArray.to(collection.mutable.Buffer)
        val u = b(n.value)
        b.update(n.value, v.asInstanceOf[Object])
        (u, Tuple.fromArray(b.to(Array))).asInstanceOf[Out]
      }
    }
}
