package typify
package tuple

type ReplaceElem[L <: Tuple, U, V] = L match {
  case h *: t => Invariant[h] match {
    case Invariant[U] => V *: t
    case _ => h *: ReplaceElem[t, U, V]
  }
}

type ReplaceAtIndex[L <: Tuple, I <: Int, A] = ReplaceAtIndex0[L, I, A, 0]

type ReplaceAtIndex0[L <: Tuple, I <: Int, A, Curr <: Int] <: Tuple = (L, Curr) match {
  case (_ *: t, I) => A *: t
  case (h *: t, _) => h *: ReplaceAtIndex0[t, I, A, compiletime.ops.int.S[Curr]]
}

/**
 * Type class supporting replacement of the first element of type `U` from this `Tuple` with an element of type `V`.
 * Available only if this `Tuple` contains an element of type `U`.
 */
trait Replacer[L, U, V] extends DepFn2[L, V] with Serializable

object Replacer {
  type Aux[L, U, V, O] = Replacer[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Replacer[L, U, V]): Replacer.Aux[L, U, V, r.Out] = r

  given replacerTuple[L <: Tuple, U, V](
    using idx: ValueOf[ElemIndex[L, U]],
  ): Replacer.Aux[L, U, V, (U, ReplaceElem[L, U, V])] =
    new Replacer[L, U, V] {
      type Out = (U, ReplaceElem[L, U, V])
      def apply(l: L, v: V): Out = {
        val b = l.toArray.to(collection.mutable.Buffer)
        val u = b(idx.value)
        b.update(idx.value, v.asInstanceOf[Object])
        (u, Tuple.fromArray(b.to(Array))).asInstanceOf[Out]
      }
    }
}
