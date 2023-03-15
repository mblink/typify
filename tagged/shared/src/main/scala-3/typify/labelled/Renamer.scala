package typify
package labelled

trait Renamer[T <: Tuple, K1, K2] {
  type Out
  def apply(t: T): Out
}

object Renamer {
  inline def apply[T <: Tuple, K1, K2](using r: Renamer[T, K1, K2]): Aux[T, K1, K2, r.Out] = r

  type Aux[T <: Tuple, K1, K2, Out0] = Renamer[T, K1, K2] { type Out = Out0 }

  given tupleRenamer1[T <: Tuple, K1, K2, V]: Aux[(K1 ->> V) *: T, K1, K2, (K2 ->> V) *: T] =
    new Renamer[(K1 ->> V) *: T, K1, K2] {
      type Out = (K2 ->> V) *: T
      def apply(l: (K1 ->> V) *: T): Out = label[K2](l.head: V) *: (l.tail: T)
    }

  given tupleRenamer[H, T <: Tuple, K1, K2, V](using t: Renamer[T, K1, K2] { type Out <: Tuple }): Aux[H *: T, K1, K2, H *: t.Out] =
    new Renamer[H *: T, K1, K2] {
      type Out = H *: t.Out
      def apply(l: H *: T): Out = l.head *: t(l.tail)
    }
}
