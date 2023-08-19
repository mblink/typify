package typify.tuple

trait Transposer[L] extends DepFn1[L]

object Transposer {
  type Aux[L, O] = Transposer[L] { type Out = O }

  inline def apply[L](implicit t: Transposer[L]): Transposer.Aux[L, t.Out] = t

  given emptyTupleTransposer: Transposer.Aux[EmptyTuple, EmptyTuple] =
    new Transposer[EmptyTuple] {
      type Out = EmptyTuple
      def apply(l: EmptyTuple): Out = l
    }

  given tupleTransposer1[H <: Tuple, MC <: Tuple, Out0 <: Tuple](
    using mc: ConstMapper.Aux[EmptyTuple, H, MC],
    zo: ZipOne.Aux[H, MC, Out0],
  ): Transposer.Aux[H *: EmptyTuple, Out0] =
    new Transposer[H *: EmptyTuple] {
      type Out = Out0
      def apply(l: H *: EmptyTuple): Out = zo(l.head, mc(EmptyTuple, l.head))
    }

  given tupleTransposer2[H <: Tuple, TH <: Tuple, TT <: Tuple, OutT <: Tuple, Out0 <: Tuple](
    using tt: Transposer.Aux[TH *: TT, OutT],
    zo: ZipOne.Aux[H, OutT, Out0],
  ): Transposer.Aux[H *: TH *: TT, Out0] =
    new Transposer[H *: TH *: TT] {
      type Out = Out0
      def apply(l: H *: TH *: TT): Out = zo(l.head, tt(l.tail))
    }
}
