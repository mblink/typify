package typify.tuple

/**
 * Type class supporting `Tuple` intersection. In case of duplicate types, this operation is a multiset intersection.
 * If type `T` appears n times in this `Tuple` and m < n times in `M`, the resulting `Tuple` contains the first m
 * elements of type `T` in this `Tuple`.
 *
 * Also available if `M` contains types absent in this `Tuple`.
 */
trait Intersection[L, M] extends DepFn1[L] with Serializable

object Intersection {
  type Aux[L, M, O] = Intersection[L, M] { type Out = O }

  inline def apply[L, M](using i: Intersection[L, M]): Intersection.Aux[L, M, i.Out] = i

  // let ∅ ∩ M = ∅
  given emptyTupleIntersection[M <: Tuple]: Intersection.Aux[EmptyTuple, M, EmptyTuple] =
    new Intersection[EmptyTuple, M] {
      type Out = EmptyTuple
      def apply(l: EmptyTuple): Out = EmptyTuple
    }

  // let (H *: T) ∩ M = T ∩ M when H ∉ M
  given tupleNIntersection0[H, T <: Tuple, M, I <: Tuple](
    using f: NotContains[M, H],
    i: Intersection.Aux[T, M, I],
  ): Intersection.Aux[H *: T, M, I] =
    new Intersection[H *: T, M] {
      type Out = I
      def apply(l: H *: T): Out = i(l.tail)
    }

  // let (H *: T) ∩ M  =  H *: (T ∩ (M - H)) when H ∈ M
  given tupleNIntersection2[H, T <: Tuple, M, MR, I <: Tuple](
    using r: Remove.Aux[M, H, (H, MR)],
    i: Intersection.Aux[T, MR, I]
  ): Intersection.Aux[H *: T, M, H *: I] =
    new Intersection[H *: T, M] {
      type Out = H *: I
      def apply(l: H *: T): Out = l.head *: i(l.tail)
    }
}
