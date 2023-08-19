package typify.tuple

trait ReversePrepend[L, R] extends DepFn2[L, R]

object ReversePrepend {
  type Aux[L, R, O] = ReversePrepend[L, R] { type Out = O }

  inline def apply[L, R](using p: ReversePrepend[L, R]): ReversePrepend.Aux[L, R, p.Out] = p

  given tupleReversePrepend[L <: Tuple, R <: Tuple]: ReversePrepend.Aux[L, R, Tuple.Concat[ReverseT[L], R]] =
    new ReversePrepend[L, R] {
      type Out = Tuple.Concat[ReverseT[L], R]
      def apply(l: L, r: R): Out = Tuple.fromArray(l.toArray.reverse ++ r.toArray).asInstanceOf[Out]
    }
}
