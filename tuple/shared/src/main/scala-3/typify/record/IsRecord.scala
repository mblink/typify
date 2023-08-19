package typify.record

type IsRecord[T <: Tuple] <: Boolean = T match {
  case EmptyTuple => true
  case (_ ->> _) *: t => IsRecord[t]
  case _ *: _ => false
}
