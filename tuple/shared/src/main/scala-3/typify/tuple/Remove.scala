package typify
package tuple

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
trait Remove[L, E] extends DepFn1[L] with Serializable {
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
      private lazy val i = idx.value
      def apply(l: L): Out = {
        val a = l.toArray
        (a(i), Tuple.fromArray(a.patch(i, Nil, 1))).asInstanceOf[Out]
      }
      def reinsert(out: Out): L =
        Tuple.fromArray(out._2.toArray.patch(i, List(out._1.asInstanceOf[Object]), 0)).asInstanceOf[L]
    }
}
