package typify
package record

/**
 * Match type to replace the value of the field at key `K` with `V` in a record of type `T`.
 */
type ReplaceValue[T <: Tuple, K, V] <: Tuple = T match {
  case (k ->> v) *: t => Invariant[k] match {
    case Invariant[K] => (K ->> V) *: t
    case _ => (k ->> v) *: ReplaceValue[t, K, V]
  }
  case h *: t => h *: ReplaceValue[t, K, V]
  case EmptyTuple => EmptyTuple
}
