package typify.tuple

import scala.collection.{Factory, mutable}

type ToArray[L, Lub] = ToTraversable.Aux[L, Array, Lub]
def ToArray[L, Lub](implicit t: ToArray[L, Lub]): ToArray[L, Lub] = t

type ToList[L, Lub] = ToTraversable.Aux[L, List, Lub]
def ToList[L, Lub](implicit t: ToList[L, Lub]): ToList[L, Lub] = t

trait ToTraversable[L, M[_]] extends DepFn1[L] {
  type Lub
  def builder(): mutable.Builder[Lub, M[Lub]]
  def append[LLub](l: L, b: mutable.Builder[LLub, M[LLub]], f: Lub => LLub): Unit

  type Out = M[Lub]
  def apply(l: L): Out = {
    val b = builder()
    append(l, b, identity)
    b.result()
  }
}

object ToTraversable {
  type Aux[L, M[_], Lub0] = ToTraversable[L, M] { type Lub = Lub0 }

  inline def apply[L, M[_]](using t: ToTraversable[L, M]): ToTraversable.Aux[L, M, t.Lub] = t

  given emptyTupleToTraversableEmptyTuple[L <: EmptyTuple, M[_], T](
    using cbf: Factory[T, M[T]],
  ): ToTraversable.Aux[L, M, T] =
    new ToTraversable[L, M] {
      type Lub = T
      def builder() = cbf.newBuilder
      def append[LLub](l: L, b: mutable.Builder[LLub, M[LLub]], f: Lub => LLub) = ()
    }

  given emptyTupleToTraversable[L <: EmptyTuple, M[_]](
    using cbf: Factory[Nothing, M[Nothing]],
  ): ToTraversable.Aux[L, M, Nothing] =
    emptyTupleToTraversableEmptyTuple[L, M, Nothing]

  given tuple1ToTraversable[T, M[_], Lub0](
    using ev: T <:< Lub0,
    cbf: Factory[Lub0, M[Lub0]],
  ): ToTraversable.Aux[T *: EmptyTuple, M, Lub0] =
    new ToTraversable[T *: EmptyTuple, M] {
      type Lub = Lub0
      def builder() = cbf.newBuilder
      def append[LLub](l: T *: EmptyTuple, b: mutable.Builder[LLub, M[LLub]], f: Lub0 => LLub) = b += f(l.head)
    }

  given tupleNToTraversable[H1, H2, T <: Tuple, LubT, Lub0, M[_]](
    using tttvs: ToTraversable.Aux[H2 *: T, M, LubT],
    u: Lub[H1, LubT, Lub0],
    cbf: Factory[Lub0, M[Lub0]],
  ): ToTraversable.Aux[H1 *: H2 *: T, M, Lub0] =
    new ToTraversable[H1 *: H2 *: T, M] {
      type Lub = Lub0
      def builder() = cbf.newBuilder
      def append[LLub](l: H1 *: H2 *: T, b: mutable.Builder[LLub, M[LLub]], f: Lub0 => LLub): Unit = {
        b += f(u.left(l.head))
        tttvs.append[LLub](l.tail, b, f compose u.right)
      }
    }
}
