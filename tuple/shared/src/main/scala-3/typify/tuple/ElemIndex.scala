package typify.tuple

type ElemIndex[T <: Tuple, A] = ElemIndex0[T, A, 0]

type ElemIndex0[T <: Tuple, A, I <: Int] <: Int = T match {
  case A *: _ => I
  case _ *: t => ElemIndex0[t, A, compiletime.ops.int.S[I]]
}
