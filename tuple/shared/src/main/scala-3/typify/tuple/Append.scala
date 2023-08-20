package typify.tuple

/**
 * Match type to append an element `A` to a `Tuple` of type `T`
 */
type AppendT[T <: Tuple, A] <: Tuple = T match {
  case EmptyTuple => A *: EmptyTuple
  case h *: t => h *: AppendT[t, A]
}
