package typify.record

import typify.tuple.DepFn0

type KeysT[T <: Tuple] <: Tuple = T match {
  case (k ->> _) *: t => k *: KeysT[t]
  case EmptyTuple => EmptyTuple
}

/**
 * Type class supporting collecting the keys of a record as a `Tuple`.
 */
trait Keys[T <: Tuple] extends DepFn0 { final type Out = KeysT[T] }

object Keys {
  type Aux[T <: Tuple, Out0] = Keys[T] { type Out = Out0 }

  inline def apply[T <: Tuple](using k: Keys[T]): Keys.Aux[T, k.Out] = k

  inline given inst[T <: Tuple]: Keys.Aux[T, KeysT[T]] =
    new Keys[T] {
      def apply(): KeysT[T] =
        compiletime.summonAll[Tuple.Map[KeysT[T], ValueOf]]
          .map[[a] =>> Any]([t] => (t: t) => t.asInstanceOf[ValueOf[_]].value)
          .asInstanceOf[KeysT[T]]
    }
}
