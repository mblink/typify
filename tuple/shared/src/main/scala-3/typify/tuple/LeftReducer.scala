package typify.tuple

trait LeftReducer[L <: Tuple, F] extends DepFn1[L]

object LeftReducer {
  type Aux[L <: Tuple, F, O] = LeftReducer[L, F] { type Out = O }

  inline def apply[L <: Tuple, F](using r: LeftReducer[L, F]): LeftReducer.Aux[L, F, r.Out] = r

  given leftReducerTuple[H, T <: Tuple, F, O](using f: LeftFolder.Aux[T, H, F, O]): LeftReducer.Aux[H *: T, F, O] =
    new LeftReducer[H *: T, F] {
      type Out = O
      def apply(l: H *: T): Out = f(l.tail, l.head)
    }
}
