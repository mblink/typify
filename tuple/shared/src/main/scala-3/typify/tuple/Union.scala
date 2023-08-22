package typify.tuple

/**
 * Type class supporting `Tuple` union. In case of duplicate types, this operation is a order-preserving multi-set union.
 * If type `T` appears n times in this `Tuple` and m > n times in `M`, the resulting `Tuple` contains the first n elements
 * of type `T` in this `Tuple`, followed by the last m - n element of type `T` in `M`.
 */
trait Union[L, M] extends DepFn2[L, M] with Serializable

object Union {
  type Aux[L, M, O] = Union[L, M] { type Out = O }

  inline def apply[L, M](using u: Union[L, M]): Union.Aux[L, M, u.Out] = u

  // let ∅ ∪ M = M
  given emptyTupleUnion[M <: Tuple]: Union.Aux[EmptyTuple, M, M] =
    new Union[EmptyTuple, M] {
      type Out = M
      def apply(l: EmptyTuple, m: M): Out = m
    }

  // let (H *: T) ∪ M = H *: (T ∪ M) when H ∉ M
  given tupleNUnion1[H, T <: Tuple, M, U <: Tuple](
    using f: NotContains[M, H],
    u: Union.Aux[T, M, U]
  ): Union.Aux[H *: T, M, H *: U] =
    new Union[H *: T, M] {
      type Out = H *: U
      def apply(l: H *: T, m: M): Out = l.head *: u(l.tail, m)
    }

  // let (H *: T) ∪ M  =  H *: (T ∪ (M - H)) when H ∈ M
  given tulpeNUnion2[H, T <: Tuple, M, MR, U <: Tuple](
    using r: Remove.Aux[M, H, (H, MR)],
    u: Union.Aux[T, MR, U]
  ): Union.Aux[H *: T, M, H *: U] =
    new Union[H *: T, M] {
      type Out = H *: U
      def apply(l: H *: T, m: M): Out = l.head *: u(l.tail, r(m)._2)
    }
}
