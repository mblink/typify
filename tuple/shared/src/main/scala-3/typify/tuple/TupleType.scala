package typify.tuple

/**
 * Match type that will only reduce if `A` is a subtype of `Tuple`.
 */
type TupleType[A] <: Tuple = A match {
  case EmptyTuple => EmptyTuple
  case h *: t => h *: t
}
