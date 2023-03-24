package typify
package labelled

type StringKey[K <: String] = K

type KeysT[T <: Tuple] <: Tuple = T match {
  case (StringKey[k] ->> v) *: t => k *: KeysT[t]
  case EmptyTuple => EmptyTuple
}

trait Keys[T <: Tuple] {
  final type Out = KeysT[T]
  def apply(): Out
}

object Keys {
  inline def apply[T <: Tuple]: Keys[T] =
    new Keys[T] {
      def apply(): KeysT[T] = compiletime.constValueTuple[KeysT[T]]
    }

  inline given inst[T <: Tuple]: Keys[T] =
    apply[T]
}
