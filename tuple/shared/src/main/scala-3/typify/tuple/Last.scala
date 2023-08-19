package typify.tuple

trait Last[T] extends DepFn1[T]

object Last {
  type Aux[T, O] = Last[T] { type Out = O }

  inline def apply[T](using l: Last[T]): Last.Aux[T, l.Out] = l

  given lastNonEmptyTuple[T <: NonEmptyTuple]: Last.Aux[T, Tuple.Last[T]] =
    new Last[T] {
      type Out = Tuple.Last[T]
      def apply(t: T): Out = t.toArray.last.asInstanceOf[Out]
    }
}
