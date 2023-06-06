package typify.tuple

sealed trait TupleSelector[T <: Tuple, A] extends (T => A)

object TupleSelector {
  inline given tupleSelectorHead[H, T <: Tuple]: TupleSelector[H *: T, H] =
    new TupleSelector[H *: T, H] { def apply(t: H *: T): H = t.head }

  inline given tupleSelectorTail[A, H, T <: Tuple](using s: TupleSelector[T, A]): TupleSelector[H *: T, A] =
    new TupleSelector[H *: T, A] { def apply(t: H *: T): A = s(t.tail) }
}
