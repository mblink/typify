package typify.record

import typify.tuple.DepFn1

type FieldsT[T <: Tuple] <: Tuple = T match {
  case (k ->> v) *: t => (k, v) *: FieldsT[t]
  case EmptyTuple => EmptyTuple
}

/**
 * Type class supporting converting this record to a `Tuple` of key-value pairs.
 */
trait Fields[T <: Tuple] extends DepFn1[T] with Serializable { final type Out = FieldsT[T] }

object Fields {
  type Aux[T <: Tuple, O] = Fields[T] { type Out = O }

  inline def apply[T <: Tuple](using v: Fields[T]): Fields.Aux[T, v.Out] = v

  inline given tupleFields[T <: Tuple](using ev: IsRecord[T] =:= true): Fields.Aux[T, FieldsT[T]] =
    new Fields[T] {
      def apply(t: T): FieldsT[T] = Keys[T]().zip(t).asInstanceOf[FieldsT[T]]
    }
}
