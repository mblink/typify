package typify.tuple

type Reverse[T <: Tuple] = T match {
  case EmptyTuple => EmptyTuple
  case h *: t => Append[Reverse[t], h]
}
