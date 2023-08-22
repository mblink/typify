package typify.tuple

import compiletime.ops.int.{+, >=}

type RangeT[N <: Int, M <: Int] <: Tuple = N match {
  case M => EmptyTuple
  case _ => N *: RangeT[N + 1, M]
}

/**
 * Type class supporting the patching of a `Tuple`.
 */
trait Range[N, M] extends DepFn0 with Serializable

object Range {
  type Aux[N, M, O] = Range[N, M] { type Out = O }

  inline def apply[N, M](using r: Range[N, M]): Range.Aux[N, M, r.Out] = r

  inline given rangeInst[N <: Int, M <: Int](
    using ev: (M >= N) =:= true,
  ): Range.Aux[N, M, RangeT[N, M]] =
    new Range[N, M] {
      type Out = RangeT[N, M]
      def apply(): Out = compiletime.constValueTuple[Out]
    }
}
