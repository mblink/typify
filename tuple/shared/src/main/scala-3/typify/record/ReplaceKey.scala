package typify
package record

/**
 * Match type to replace key `K1` with `K2` in a record of type `T`.
 */
type ReplaceKey[T <: Tuple, K1, K2] <: Tuple = T match {
  case (k ->> v) *: t => Invariant[k] match {
    case Invariant[K1] => (K2 ->> v) *: t
    case _ => (k ->> v) *: ReplaceKey[t, K1, K2]
  }
  case h *: t => h *: ReplaceKey[t, K1, K2]
  case EmptyTuple => EmptyTuple
}
