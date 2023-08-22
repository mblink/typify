package typify.tuple

type ReverseT[T <: Tuple] <: Tuple = T match {
  case EmptyTuple => EmptyTuple
  case h *: t => AppendT[ReverseT[t], h]
}

/**
 * Type class supporting reversing this `Tuple`.
 */
trait Reverse[T] extends DepFn1[T] with Serializable

object Reverse {
  type Aux[T, O] = Reverse[T] { type Out = O }

  inline def apply[T](using r: Reverse[T]): Reverse.Aux[T, r.Out] = r

  given reverseTuple[T <: Tuple]: Reverse.Aux[T, ReverseT[T]] =
    new Reverse[T] {
      type Out = ReverseT[T]
      def apply(t: T): Out = Tuple.fromArray(t.toArray.reverse).asInstanceOf[Out]
    }
}
