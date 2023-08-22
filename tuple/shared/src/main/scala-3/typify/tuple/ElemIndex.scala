package typify.tuple

import compiletime.ops.int.S

/**
 * Match type to find the first index at which type `A` appears in a `Tuple` of type `T`.
 */
type ElemIndex[T <: Tuple, A] = ElemIndex0[T, A, 0]

private[tuple] type ElemIndex0[T <: Tuple, A, I <: Int] <: Int = T match {
  case h *: t => Invariant[h] match {
    case Invariant[A] => I
    case _ => ElemIndex0[t, A, S[I]]
  }
}

/**
 * Match type to find the first index at which type `A` appears in a `Tuple` of type `T`,
 * or `-1` if `A` does not appear in `T`.
 */
type ElemIndexWithFallback[T <: Tuple, E] = ElemIndexWithFallback0[T, E, 0]

private[tuple] type ElemIndexWithFallback0[T <: Tuple, E, I <: Int] <: Int = T match {
  case h *: t => Invariant[h] match {
    case Invariant[E] => I
    case _ => ElemIndexWithFallback0[t, E, S[I]]
  }
  case EmptyTuple => -1
}
