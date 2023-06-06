package typify.tuple

import scala.collection.mutable

trait ToList[L <: Tuple, Lub] extends DepFn1[L] {
  def append[LLub](l: L, b: mutable.Builder[LLub, List[LLub]], f: Lub => LLub): Unit

  final type Out = List[Lub]

  final def apply(l: L): Out = {
    val b = List.newBuilder[Lub]
    append(l, b, identity)
    b.result()
  }
}

object ToList {
  inline def apply[L <: Tuple, Lub](using t: ToList[L, Lub]): ToList[L, Lub] = t

  given toListEmptyTuple[L <: EmptyTuple, T]: ToList[L, T] =
    new ToList[L, T] {
      def append[LLub](l: L, b: mutable.Builder[LLub, List[LLub]], f: T => LLub) = ()
    }

  given toListEmptyTupleNothing[L <: EmptyTuple]: ToList[L, Nothing] = toListEmptyTuple[L, Nothing]

  given toListTuple1[T, Lub0](using ev: T <:< Lub0): ToList[T *: EmptyTuple, Lub0] =
    new ToList[T *: EmptyTuple, Lub0] {
      def append[LLub](l: T *: EmptyTuple, b: mutable.Builder[LLub, List[LLub]], f: Lub0 => LLub) = b += f(l.head)
    }

  given toListTupleN[H1, H2, T <: Tuple, LubT, Lub0](using h: Lub[H1, LubT, Lub0], t: ToList[H2 *: T, LubT]): ToList[H1 *: H2 *: T, Lub0] =
    new ToList[H1 *: H2 *: T, Lub0] {
      def append[LLub](l: H1 *: H2 *: T, b: mutable.Builder[LLub, List[LLub]], f: Lub0 => LLub): Unit = {
        b += f(h.left(l.head))
        t.append[LLub](l.tail, b, f.compose(h.right))
      }
    }
}
