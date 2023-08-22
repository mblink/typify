package typify.tuple

/**
 * Type class supporting `Tuple` subtraction. In case of duplicate types, this operation is a multiset difference.
 * If type `T` appears n times in this `Tuple` and m < n times in `M`, the resulting `Tuple` contains the last n - m
 * elements of type `T` in this `Tuple`.
 *
 * Also available if `M` contains types absent in this `Tuple`.
 *
 * @author Olivier Blanvillain
 */
trait Diff[L, M] extends DepFn1[L]

sealed trait DiffLP {
  final type Aux[L, M, O] = Diff[L, M] { type Out = O }

  final given tupleNDiff1[L, H, T <: Tuple, D <: Tuple](using d: Diff.Aux[L, T, D]): Diff.Aux[L, H *: T, D] =
    new Diff[L, H *: T] {
      type Out = D
      def apply(l: L): Out = d(l)
    }
}

object Diff extends DiffLP {
  inline def apply[L, M](implicit d: Diff[L, M]): Diff.Aux[L, M, d.Out] = d

  given emptyTupleDiff[L]: Diff.Aux[L, EmptyTuple, L] =
    new Diff[L, EmptyTuple] {
      type Out = L
      def apply(l: L): Out = l
    }

  given tupleNDiff2[L <: Tuple, LT, H, T <: Tuple, D](
    using r: Remove.Aux[L, H, (H, LT)],
    d: Diff.Aux[LT, T, D]
  ): Diff.Aux[L, H *: T, D] =
    new Diff[L, H *: T] {
      type Out = D
      def apply(l: L): Out = d(r(l)._2)
    }
}
