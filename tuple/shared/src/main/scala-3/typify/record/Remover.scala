package typify.record

import typify.tuple.DepFn1

trait Remover[T <: Tuple, K] extends DepFn1[T]

type ReversePrependTuple[L <: Tuple, M <: Tuple] <: Tuple = L match {
  case EmptyTuple => M
  case h *: t => ReversePrependTuple[t, h *: M]
}

object Remover {
  type Aux[T <: Tuple, K, O] = Remover[T, K] { type Out = O }

  inline def apply[T <: Tuple, K](implicit r: Remover[T, K]): Remover.Aux[T, K, r.Out] = r
  inline def apply[T <: Tuple, K](t: T, k: K)(implicit r: Remover[T, K]): r.Out = r(t)

  type RemoveField[T <: Tuple, K] = RemoveField0[T, K, EmptyTuple]

  type RemoveField0[T <: Tuple, K, Acc <: Tuple] <: (Any, Tuple) = T match {
    case (K ->> v) *: t => (v, ReversePrependTuple[Acc, t])
    case h *: t => RemoveField0[t, K, h *: Acc]
  }

  inline given removerInst[T <: Tuple, K](
    using idx: ValueOf[FindFieldIndex[T, K]],
  ): Remover.Aux[T, K, RemoveField[T, K]] =
    new Remover[T, K] {
      type Out = RemoveField[T, K]
      def apply(t: T): Out = {
        val b = t.toArray.to(collection.mutable.Buffer)
        val v = b.remove(idx.value)
        (v, Tuple.fromArray(b.to(Array))).asInstanceOf[RemoveField[T, K]]
      }
    }
}
