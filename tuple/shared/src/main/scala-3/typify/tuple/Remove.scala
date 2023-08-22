package typify.tuple

type RemoveT[L <: Tuple, E] <: Tuple = L match {
  case h *: t => Invariant[h] match {
    case Invariant[E] => t
    case _ => h *: RemoveT[t, E]
  }
  case EmptyTuple => EmptyTuple
}

/**
 * Type class supporting removal of an element from this `Tuple`. Available only if this `Tuple` contains an
 * element of type `E`.
 */
trait Remove[L, E] extends DepFn1[L] {
  def reinsert(out: Out): L
}

object Remove {
  type Aux[L, E, O] = Remove[L, E] { type Out = O }

  inline def apply[L, E](using r: Remove[L, E]): Remove.Aux[L, E, r.Out] = r

  given removeTuple[L <: Tuple, E](
    using idx: ValueOf[ElemIndex[L, E]],
  ): Remove.Aux[L, E, (E, RemoveT[L, E])] =
    new Remove[L, E] {
      type Out = (E, RemoveT[L, E])
      def apply(l: L): Out = {
        val b = l.toArray.to(collection.mutable.Buffer)
        val v = b.remove(idx.value)
        (v, Tuple.fromArray(b.to(Array))).asInstanceOf[Out]
      }
      def reinsert(out: Out): L = {
        val b = out._2.toArray.to(collection.mutable.Buffer)
        b.insert(idx.value, out._1.asInstanceOf[Object])
        Tuple.fromArray(b.to(Array)).asInstanceOf[L]
      }
    }
}
