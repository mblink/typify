package typify.tuple

/**
 * Type class supporting replacement of the `N`th element of this `Tuple` with the result of calling `F` on it.
 * Available only if this `Tuple` contains at least `N` elements.
 */
trait ModifierAt[L, N, U, V] extends DepFn2[L, U => V]

object ModifierAt {
  type Aux[L, N, U, V, O] = ModifierAt[L, N, U, V] { type Out = O }

  inline def apply[L, N, U, V](using r: ModifierAt[L, N, U, V]): ModifierAt.Aux[L, N, U, V, r.Out] = r

  given modifierAtTuple[L <: Tuple, N <: Int, U, V](
    using ev: Tuple.Elem[L, N] <:< U,
    n: ValueOf[N],
  ): ModifierAt.Aux[L, N, U, V, (U, ReplaceAtIndex[L, N, V])] =
    new ModifierAt[L, N, U, V] {
      type Out = (U, ReplaceAtIndex[L, N, V])
      def apply(l: L, f: U => V): Out = {
        val b = l.toArray.to(collection.mutable.Buffer)
        val u = b(n.value)
        b.update(n.value, f(u.asInstanceOf[U]).asInstanceOf[Object])
        (u, Tuple.fromArray(b.to(Array))).asInstanceOf[Out]
      }
    }
}
