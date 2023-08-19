package typify.tuple

import scala.util.NotGiven

trait SubtypeUnifier[T, B] extends DepFn1[T]

object SubtypeUnifier {
  type Aux[T, B, O] = SubtypeUnifier[T, B] { type Out = O }

  inline def apply[T, B](using u: SubtypeUnifier[T, B]): SubtypeUnifier.Aux[T, B, u.Out] = u

  given subtypeUnifierEmptyTuple[B]: SubtypeUnifier.Aux[EmptyTuple, B, EmptyTuple] =
    new SubtypeUnifier[EmptyTuple, B] {
      type Out = EmptyTuple
      def apply(l: EmptyTuple): Out = l
    }

  given subtypeUnifierTupleN1[H, T <: Tuple, B, SutOut <: Tuple](
    using st: H <:< B,
    sut: SubtypeUnifier.Aux[T, B, SutOut],
  ): SubtypeUnifier.Aux[H *: T, B, B *: SutOut] =
    new SubtypeUnifier[H *: T, B] {
      type Out = B *: SutOut
      def apply(l: H *: T): Out = st(l.head) *: sut(l.tail)
    }

  given subtypeUnifierTupleN1[H, T <: Tuple, B, SutOut <: Tuple](
    using nst: NotGiven[H <:< B],
    sut: SubtypeUnifier.Aux[T, B, SutOut],
  ): SubtypeUnifier.Aux[H *: T, B, H *: SutOut] =
    new SubtypeUnifier[H *: T, B] {
      type Out = H *: SutOut
      def apply(l: H *: T): Out = l.head *: sut(l.tail)
    }
}
