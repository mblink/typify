package typify.tuple

trait Prepend[L, R] extends DepFn2[L, R]

trait PrependLP1 {
  final type Aux[L, R, O] = Prepend[L, R] { type Out = O }

  final given prependTupleCons[LH, LT <: Tuple, R <: Tuple, OT <: Tuple](
    using pt: Prepend.Aux[LT, R, OT],
  ): Prepend.Aux[LH *: LT, R, LH *: OT] =
    new Prepend[LH *: LT, R] {
      type Out = LH *: OT
      def apply(l: LH *: LT, r: R): Out = l.head *: pt(l.tail, r)
    }
}

trait PrependLP0 extends PrependLP1 {
  final given prependEmptyTuple0[L <: Tuple, R <: EmptyTuple]: Prepend.Aux[L, R, L] =
    new Prepend[L, R] {
      type Out = L
      def apply(l: L, r: R): L = l
    }
}

object Prepend extends PrependLP0 {
  inline def apply[L, R](using p: Prepend[L, R]): Prepend.Aux[L, R, p.Out] = p

  given prependEmptyTuple1[L <: EmptyTuple, R <: Tuple]: Prepend.Aux[L, R, R] =
    new Prepend[L, R] {
      type Out = R
      def apply(l: L, r: R): R = r
    }
}
