package typify.tuple

trait Modifier[L, U, V] extends DepFn2[L, U => V]

object Modifier {
  type Aux[L, U, V, O] = Modifier[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Modifier[L, U, V]): Modifier.Aux[L, U, V, r.Out] = r

  given modifierTuple[L <: Tuple, U, V](
    using idx: ValueOf[ElemIndex[L, U]],
  ): Modifier.Aux[L, U, V, (U, ReplacerT[L, U, V])] =
    new Modifier[L, U, V] {
      type Out = (U, ReplacerT[L, U, V])
      def apply(l: L, f: U => V): Out = {
        val b = l.toArray.to(collection.mutable.Buffer)
        val u = b(idx.value)
        b.update(idx.value, f(u.asInstanceOf[U]).asInstanceOf[Object])
        (u, Tuple.fromArray(b.to(Array))).asInstanceOf[Out]
      }
    }
}
