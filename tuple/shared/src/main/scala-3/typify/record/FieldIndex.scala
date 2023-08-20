package typify.record

/**
 * Match type to find the first index which key `K` appears in a record of type `T`.
 */
type FieldIndex[T <: Tuple, K] = FieldIndex0[T, K, 0]

type FieldIndex0[T <: Tuple, K, I <: Int] <: Int = T match {
  case (K ->> _) *: _ => I
  case _ *: t => FieldIndex0[t, K, compiletime.ops.int.S[I]]
}

/**
 * Match type to find the value of the first field with key `K` in a record of type `T`.
 */
type FieldValue[T <: Tuple, K] <: Any = T match {
  case (K ->> v) *: _ => v
  case _ *: t => FieldValue[t, K]
}
