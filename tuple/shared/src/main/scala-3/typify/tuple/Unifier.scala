package typify.tuple

/**
 * Type class supporting unification of this `Tuple`.
 */
trait Unifier[T] extends DepFn1[T]

object Unifier {
  type Aux[T, O] = Unifier[T] { type Out = O }

  inline def apply[T](using u: Unifier[T]): Unifier.Aux[T, u.Out] = u

  given unifierEmptyTuple: Unifier.Aux[EmptyTuple, EmptyTuple] =
    new Unifier[EmptyTuple] {
      type Out = EmptyTuple
      def apply(l: EmptyTuple): Out = l
    }

  given unifierTuple1[T]: Unifier.Aux[T *: EmptyTuple, T *: EmptyTuple] =
    new Unifier[T *: EmptyTuple] {
      type Out = T *: EmptyTuple
      def apply(l: T *: EmptyTuple): Out = l
    }

  given unifierTupleN[H1, H2, L, T <: Tuple, LtOut <: Tuple](
    using lub: Lub[H1, H2, L],
    ut: Unifier.Aux[L *: T, LtOut],
  ): Unifier.Aux[H1 *: H2 *: T, L *: LtOut] =
    new Unifier[H1 *: H2 *: T] {
      type Out = L *: LtOut
      def apply(l: H1 *: H2 *: T): Out = lub.left(l.head) *: ut(lub.right(l.tail.head) *: l.tail.tail)
    }
}
