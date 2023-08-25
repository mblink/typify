package typify.tuple

/**
 * Type class supporting access to all but the last element of this `Tuple`. Available only if this `Tuple` has at least one element.
 */
trait Init[T] extends DepFn1[T] with Serializable

object Init {
  type Aux[T, O] = Init[T] { type Out = O }

  inline def apply[T](using l: Init[T]): Init.Aux[T, l.Out] = l

  given lastNonEmptyTuple[T <: NonEmptyTuple]: Init.Aux[T, Tuple.Init[T]] =
    new Init[T] {
      type Out = Tuple.Init[T]
      def apply(t: T): Out = t.init.asInstanceOf[Out]
    }
}
