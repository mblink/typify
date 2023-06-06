package typify.labelled

type FindField[T <: Tuple, K] = FindField0[T, K, 0]

type FindField0[T <: Tuple, K, I <: Int] <: (Any, Int) = T match {
  case (K ->> f) *: _ => (f, I)
  case _ *: t => FindField0[t, K, compiletime.ops.int.S[I]]
}
