package typify
package record

import typify.tuple.DepFn1

/**
 * Type class supporting record field removal.
 */
trait Remover[T, K] extends DepFn1[T]

type ReversePrependTuple[L <: Tuple, M <: Tuple] <: Tuple = L match {
  case EmptyTuple => M
  case h *: t => ReversePrependTuple[t, h *: M]
}

object Remover {
  type Aux[T, K, O] = Remover[T, K] { type Out = O }

  inline def apply[T, K](using r: Remover[T, K]): Remover.Aux[T, K, r.Out] = r
  inline def apply[T, K](t: T, k: K)(using r: Remover[T, K]): r.Out = r(t)

  type RemoveField[T <: Tuple, K] <: Tuple = T match {
    case (k ->> v) *: t => Invariant[k] match {
      case Invariant[K] => t
      case _ => (k ->> v) *: RemoveField[t, K]
    }
    case h *: t => h *: RemoveField[t, K]
  }

  inline given tupleRemover[T <: Tuple, K](
    using idx: ValueOf[FieldIndex[T, K]],
  ): Remover.Aux[T, K, (FieldValue[T, K], RemoveField[T, K])] =
    new Remover[T, K] {
      type Out = (FieldValue[T, K], RemoveField[T, K])
      def apply(t: T): Out = {
        val b = t.toArray.to(collection.mutable.Buffer)
        val v = b.remove(idx.value)
        (v, Tuple.fromArray(b.to(Array))).asInstanceOf[Out]
      }
    }
}
