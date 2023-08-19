package typify.tuple

trait CollectFirst[L, F] extends DepFn1[L] with Serializable

trait CollectFirstLP {
  final type Aux[L, F, O] = CollectFirst[L, F] { type Out = O }

  final given tupleIterate[H, T <: Tuple, F, O](using c: CollectFirst.Aux[T, F, O]): CollectFirst.Aux[H *: T, F, O] =
    new CollectFirst[H *: T, F] {
      type Out = O
      def apply(l: H *: T) = c(l.tail)
    }
}

object CollectFirst extends CollectFirstLP {
  inline def apply[L, F](using c: CollectFirst[L, F]): CollectFirst.Aux[L, F, c.Out] = c

  given tupleEval[H, T <: Tuple, F, O](using c: Case1[F, H, O]): CollectFirst.Aux[H *: T, F, O] =
    new CollectFirst[H *: T, F] {
      type Out = O
      def apply(l: H *: T) = c.run(l.head)
    }
}