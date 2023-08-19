package typify.tuple

trait Length[T] extends DepFn0 { type Out <: Int }

object Length {
  type Aux[T, O <: Int] = Length[T] { type Out = O }

  inline def apply[T](using l: Length[T]): Length.Aux[T, l.Out] = l

  given lengthTuple[T <: Tuple](using size: ValueOf[Tuple.Size[T]]): Length.Aux[T, Tuple.Size[T]] =
    new Length[T] {
      type Out = Tuple.Size[T]
      def apply(): Out = size.value
    }
}
