package typify.record

import typify.tuple.DepFn0

type StringKey[K <: String] = K

type KeysT[T <: Tuple] <: Tuple = T match {
  case (StringKey[k] ->> _) *: t => k *: KeysT[t]
  case EmptyTuple => EmptyTuple
}

trait Keys[T <: Tuple] extends DepFn0 { final type Out = KeysT[T] }

object Keys {
  type Aux[T <: Tuple, Out0] = Keys[T] { type Out = Out0 }

  inline def apply[T <: Tuple]: Aux[T, KeysT[T]] =
    new Keys[T] {
      def apply(): KeysT[T] = compiletime.constValueTuple[KeysT[T]]
    }

  inline given inst[T <: Tuple]: Aux[T, KeysT[T]] =
    apply[T]
}
