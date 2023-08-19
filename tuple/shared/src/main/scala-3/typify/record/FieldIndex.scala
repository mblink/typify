package typify.record

type FieldIndex[T <: Tuple, K] = FieldIndex0[T, K, 0]

type FieldIndex0[T <: Tuple, K, I <: Int] <: Int = T match {
  case (K ->> _) *: _ => I
  case _ *: t => FieldIndex0[t, K, compiletime.ops.int.S[I]]
}

type FieldValue[T <: Tuple, K] <: Any = T match {
  case (K ->> v) *: _ => v
  case _ *: t => FieldValue[t, K]
}
