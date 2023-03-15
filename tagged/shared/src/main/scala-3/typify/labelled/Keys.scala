package typify
package labelled

trait Keys[T <: Tuple] {
  type Out
  def apply(): Out
}

object Keys {
  inline def apply[T <: Tuple](using k: Keys[T]): Aux[T, k.Out] = k

  type Aux[T <: Tuple, Out0] = Keys[T] { type Out = Out0 }

  given emptyTuple: Aux[EmptyTuple, EmptyTuple] =
    new Keys[EmptyTuple] {
      type Out = EmptyTuple
      def apply(): Out = EmptyTuple
    }

  given tupleN[K, V, T <: Tuple](using k: ValueOf[K], t: Keys[T] { type Out <: Tuple }): Aux[(K ->> V) *: T, K *: t.Out] =
    new Keys[(K ->> V) *: T] {
      type Out = K *: t.Out
      def apply(): Out = k.value *: t()
    }
}
