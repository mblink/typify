package typify.labelled

type ReplaceValue[T <: Tuple, K, V] <: Tuple = T match {
  case (K ->> _) *: t => (K ->> V) *: t
  case h *: t => h *: ReplaceValue[t, K, V]
  case EmptyTuple => EmptyTuple
}
