package typify.tuple

import compiletime.ops.int.S

/**
 * Type class supporting the calculation of every combination of this `Tuple`
 */
trait Combinations[N, L] extends DepFn1[L]

trait CombinationsLP {
  final type Aux[N, L, O] = Combinations[N, L] { type Out = O }

  final given emptyTupleCombinations[N]: Combinations.Aux[N, EmptyTuple, EmptyTuple] =
    new Combinations[N, EmptyTuple] {
      type Out = EmptyTuple
      def apply(l: EmptyTuple): Out = EmptyTuple
    }
}

object Combinations extends CombinationsLP {
  inline def apply[N, L](using c: Combinations[N, L]): Combinations.Aux[N, L, c.Out] = c
  inline def apply[N, L](n: N, l: L)(using c: Combinations[N, L]): c.Out = c(l)

  given combination0[L]: Combinations.Aux[0, L, EmptyTuple *: EmptyTuple] =
    new Combinations[0, L] {
      type Out = EmptyTuple *: EmptyTuple
      def apply(l: L): Out = EmptyTuple *: EmptyTuple
    }

  given combinationN[N <: Int, H, T <: Tuple, C1 <: Tuple, C2 <: Tuple, CM <: Tuple, CpOut <: Tuple](
    using c1: Combinations.Aux[N, T, C1],
    c2: Combinations.Aux[S[N], T, C2],
    cm: MapCons.Aux[H, C1, CM],
    cp: Prepend.Aux[CM, C2, CpOut],
  ): Combinations.Aux[S[N], H *: T, CpOut] =
    new Combinations[S[N], H *: T] {
      type Out = cp.Out
      def apply(l: H *: T): Out = cp(cm(l.head, c1(l.tail)), c2(l.tail))
    }
}
