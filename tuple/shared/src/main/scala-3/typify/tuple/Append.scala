package typify.tuple

type Append[T <: Tuple, F] <: Tuple = T match {
  case EmptyTuple => F *: EmptyTuple
  case h *: t => h *: Append[t, F]
}
