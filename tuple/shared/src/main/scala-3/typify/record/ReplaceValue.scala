package typify.record

/**
 * Match type to replace the value of the field at key `K` with `V` in a record of type `T`.
 */
type ReplaceValue[T <: Tuple, K, V] <: Tuple = T match {
  case (K ->> _) *: t => (K ->> V) *: t
  case h *: t => h *: ReplaceValue[t, K, V]
  case EmptyTuple => EmptyTuple
}
