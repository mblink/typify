package typify.tuple

/**
 * Type class witnessing that `L` doesn't contain elements of type `U`
 */
trait NotContains[L, U]

sealed trait NotContainsLP {
  protected final val singleton = new NotContains[Any, Any] {}
  protected final def inst[T, U]: NotContains[T, U] = singleton.asInstanceOf[NotContains[T, U]]

  final given tupleNNotContains[T <: Tuple, U](using ev: ElemIndexWithFallback[T, U] =:= -1): NotContains[T, U] = inst
}

object NotContains extends NotContainsLP {
  inline def apply[L, U](using n: NotContains[L, U]): NotContains[L, U] = n

  given emptyTupleNotContains[U]: NotContains[EmptyTuple, U] = inst
}
