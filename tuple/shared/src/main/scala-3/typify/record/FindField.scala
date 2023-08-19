package typify.record

type FindFieldIndex[T <: Tuple, K] = FindFieldIndex0[T, K, 0]

type FindFieldIndex0[T <: Tuple, K, I <: Int] <: Int = T match {
  case (K ->> _) *: _ => I
  case _ *: t => FindFieldIndex0[t, K, compiletime.ops.int.S[I]]
}

type FindFieldValue[T <: Tuple, K] <: Any = T match {
  case (K ->> v) *: _ => v
  case _ *: t => FindFieldValue[t, K]
}
