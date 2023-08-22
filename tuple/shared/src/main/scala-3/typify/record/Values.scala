package typify.record

import typify.tuple.DepFn1

type ValuesT[T <: Tuple] <: Tuple = T match {
  case (_ ->> v) *: t => v *: ValuesT[t]
  case EmptyTuple => EmptyTuple
}

/**
 * Type class supporting collecting the values of a record as a `Tuple`.
 */
trait Values[T <: Tuple] extends DepFn1[T] with Serializable { final type Out = ValuesT[T] }

object Values {
  type Aux[T <: Tuple, O] = Values[T] { type Out = O }

  inline def apply[T <: Tuple](using v: Values[T]): Values.Aux[T, v.Out] = v

  given tupleValues[T <: Tuple](using ev: IsRecord[T] =:= true): Values.Aux[T, ValuesT[T]] =
    new Values[T] {
      def apply(t: T): ValuesT[T] = t.asInstanceOf[ValuesT[T]]
    }
}
