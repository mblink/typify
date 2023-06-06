package typify.labelled

type ReplaceKey[T <: Tuple, K1, K2] <: Tuple = T match {
  case (K1 ->> v) *: t => (K2 ->> v) *: t
  case h *: t => h *: ReplaceKey[t, K1, K2]
  case EmptyTuple => EmptyTuple
}
