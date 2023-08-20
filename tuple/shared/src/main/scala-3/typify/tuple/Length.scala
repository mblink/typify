package typify.tuple

/**
 * Type class supporting computing the singleton `Int` corresponding to the length of this `Tuple`.
 */
trait Length[T] extends DepFn0 { type Out <: Int }

object Length {
  type Aux[T, O <: Int] = Length[T] { type Out = O }

  inline def apply[T](using l: Length[T]): Length.Aux[T, l.Out] = l

  inline given lengthTuple[T <: Tuple]: Length.Aux[T, Tuple.Size[T]] =
    new Length[T] {
      type Out = Tuple.Size[T]
      def apply(): Out = compiletime.summonInline[ValueOf[Tuple.Size[T]]].value
    }
}
