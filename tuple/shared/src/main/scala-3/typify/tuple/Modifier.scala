package typify.tuple

/**
 * Type class supporting replacement of the first element of type `U` in this `Tuple` with the result of
 * its transformation via a given function into a new element of type `V`.
 * Available only if this `Tuple` contains an element of type `U`.
 */
trait Modifier[L, U, V] extends DepFn2[L, U => V] with Serializable

object Modifier {
  type Aux[L, U, V, O] = Modifier[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Modifier[L, U, V]): Modifier.Aux[L, U, V, r.Out] = r

  given modifierTuple[L <: Tuple, U, V](
    using idx: ValueOf[ElemIndex[L, U]],
  ): Modifier.Aux[L, U, V, (U, ReplaceElem[L, U, V])] =
    new Modifier[L, U, V] {
      type Out = (U, ReplaceElem[L, U, V])
      def apply(l: L, f: U => V): Out = {
        val b = l.toArray.to(collection.mutable.Buffer)
        val u = b(idx.value)
        b.update(idx.value, f(u.asInstanceOf[U]).asInstanceOf[Object])
        (u, Tuple.fromArray(b.to(Array))).asInstanceOf[Out]
      }
    }
}
