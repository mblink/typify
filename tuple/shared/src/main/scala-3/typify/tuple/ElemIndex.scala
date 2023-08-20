package typify.tuple

import compiletime.ops.int.S

/**
 * Match type to find the first index at which type `A` appears in a `Tuple` of type `T`.
 */
type ElemIndex[T <: Tuple, A] = ElemIndex0[T, A, 0]

type ElemIndex0[T <: Tuple, A, I <: Int] <: Int = T match {
  case A *: _ => I
  case _ *: t => ElemIndex0[t, A, S[I]]
}

/**
 * Match type to find the first index at which type `A` appears in a `Tuple` of type `T`,
 * or `-1` if `A` does not appear in `T`.
 */
type ElemIndexWithFallback[T <: Tuple, E] = ElemIndexWithFallback0[T, E, 0]

type ElemIndexWithFallback0[T <: Tuple, E, I <: Int] <: Int = T match {
  case E *: _ => I
  case _ *: t => ElemIndexWithFallback0[t, E, compiletime.ops.int.S[I]]
  case EmptyTuple => -1
}
