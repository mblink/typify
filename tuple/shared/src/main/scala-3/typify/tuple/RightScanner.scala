package typify.tuple

trait RightScanner[L, In, F] extends DepFn2[L, In]

object RightScanner {
  type Aux[L, In, F, O] = RightScanner[L, In, F] { type Out = O }

  inline def apply[L, In, F](using s: RightScanner[L, In, F]): RightScanner.Aux[L, In, F, s.Out] = s

  trait RightScanner0[L, V, F] extends DepFn2[L, V]

  object RightScanner0 {
    type Aux[L, V, F, O] = RightScanner0[L, V, F] { type Out = O }

    given tupleRightScanner0[H, H0, T <: Tuple, F, C2Result](
      using ev: Case2[F, H0, H, C2Result],
    ): RightScanner0.Aux[H *: T, H0, F, C2Result *: H *: T] =
      new RightScanner0[H *: T, H0, F] {
        type Out = C2Result *: H *: T
        def apply(l: H *: T, h: H0) = ev.run(h, l.head) *: l
      }
  }

  given emptyTupleRightScanner[In, F]: Aux[EmptyTuple, In, F, In *: EmptyTuple] =
    new RightScanner[EmptyTuple, In, F] {
      type Out = In *: EmptyTuple
      def apply(l: EmptyTuple, in: In): Out = in *: EmptyTuple
    }

  given tupleNRightScanner[H, T <: Tuple, In, F, R <: Tuple, Scan0Out <: Tuple](
    using st: Aux[T, In, F, R],
    sh: RightScanner0.Aux[R, H, F, Scan0Out],
  ): RightScanner.Aux[H *: T, In, F, Scan0Out] =
    new RightScanner[H *: T, In, F] {
      type Out = Scan0Out
      def apply(l: H *: T, in: In) = sh(st(l.tail, in), l.head)
    }
}
