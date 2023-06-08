package typify.tuple

type IndexOf[T <: Tuple, E] = IndexOf0[T, E, 0]

type IndexOf0[T <: Tuple, E, I <: Int] <: Int = T match {
  case E *: _ => I
  case _ *: t => IndexOf0[t, E, compiletime.ops.int.S[I]]
  case EmptyTuple => -1
}
