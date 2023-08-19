package typify.tuple

type ReplacerT[L <: Tuple, U, V] = L match {
  case U *: t => V *: t
  case h *: t => h *: ReplacerT[t, U, V]
}

trait Replacer[L, U, V] extends DepFn2[L, V]

object Replacer {
  type Aux[L, U, V, O] = Replacer[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Replacer[L, U, V]): Replacer.Aux[L, U, V, r.Out] = r

  given replacerTuple[L <: Tuple, U, V](
    using idx: ValueOf[ElemIndex[L, U]],
  ): Replacer.Aux[L, U, V, (U, ReplacerT[L, U, V])] =
    new Replacer[L, U, V] {
      type Out = (U, ReplacerT[L, U, V])
      def apply(l: L, v: V): Out = {
        val b = l.toArray.to(collection.mutable.Buffer)
        val u = b(idx.value)
        b.update(idx.value, v.asInstanceOf[Object])
        (u, Tuple.fromArray(b.to(Array))).asInstanceOf[Out]
      }
    }
}
