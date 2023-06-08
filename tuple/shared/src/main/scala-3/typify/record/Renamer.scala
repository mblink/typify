package typify.record

import typify.tuple.DepFn1

trait Renamer[T <: Tuple, K1, K2] extends DepFn1[T] {
  type Out
  def apply(t: T): Out
}

object Renamer {
  type Aux[T <: Tuple, K1, K2, Out0] = Renamer[T, K1, K2] { type Out = Out0 }

  inline def apply[T <: Tuple, K1, K2](using r: Renamer[T, K1, K2]): Renamer.Aux[T, K1, K2, r.Out] = r

  inline given renamerInst[T <: Tuple, K1, K2](
    using idx: ValueOf[Tuple.Elem[FindField[T, K1], 1]],
  ): Renamer.Aux[T, K1, K2, ReplaceKey[T, K1, K2]] =
    new Renamer[T, K1, K2] {
      type Out = ReplaceKey[T, K1, K2]
      def apply(t: T): Out = t.asInstanceOf[Out]
    }
}
