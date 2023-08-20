package typify.tuple

/**
 * Type class supporting access to the first element of type `U` in this `Tuple`. Available only if this `Tuple`
 * contains an element of type `U`.
 */
sealed trait Selector[T <: Tuple, A] extends (T => A)

object Selector {
  inline def apply[T <: Tuple, A](using s: Selector[T, A]): Selector[T, A] = s

  inline given tupleSelectorHead[H, T <: Tuple]: Selector[H *: T, H] =
    new Selector[H *: T, H] { def apply(t: H *: T): H = t.head }

  inline given tupleSelectorTail[A, H, T <: Tuple](using s: Selector[T, A]): Selector[H *: T, A] =
    new Selector[H *: T, A] { def apply(t: H *: T): A = s(t.tail) }
}
