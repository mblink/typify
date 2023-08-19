package typify.tuple

type TupleType[A] <: Tuple = A match {
  case EmptyTuple => EmptyTuple
  case h *: t => h *: t
}
