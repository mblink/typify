package typify.record

/**
 * Match type to check if all fields of a `Tuple` of type `T` are instances of `->>`.
 */
type IsRecord[T <: Tuple] <: Boolean = T match {
  case EmptyTuple => true
  case (_ ->> _) *: t => IsRecord[t]
}
