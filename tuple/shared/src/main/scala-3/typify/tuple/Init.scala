package typify.tuple

trait Init[T] extends DepFn1[T]

object Init {
  type Aux[T, O] = Init[T] { type Out = O }

  inline def apply[T](using l: Init[T]): Init.Aux[T, l.Out] = l

  given lastNonEmptyTuple[T <: NonEmptyTuple]: Init.Aux[T, Tuple.Init[T]] =
    new Init[T] {
      type Out = Tuple.Init[T]
      def apply(t: T): Out = Tuple.fromArray(t.toArray.init).asInstanceOf[Out]
    }
}
