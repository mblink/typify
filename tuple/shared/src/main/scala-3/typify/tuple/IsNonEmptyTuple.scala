package typify.tuple

trait IsNonEmptyTuple[L <: Tuple] extends Serializable {
  type H
  type T <: Tuple

  def head(l: L): H
  def tail(l: L): T
  def cons(h: H, t: T): L
}

object IsNonEmptyTuple {
  def apply[L <: Tuple](implicit n: IsNonEmptyTuple[L]): IsNonEmptyTuple.Aux[L, n.H, n.T] = n

  type Aux[L <: Tuple, H0, T0 <: Tuple] = IsNonEmptyTuple[L] { type H = H0; type T = T0 }

  given isNonEmptyTupleInst[H0, T0 <: Tuple]: IsNonEmptyTuple.Aux[H0 *: T0, H0, T0] =
    new IsNonEmptyTuple[H0 *: T0] {
      type H = H0
      type T = T0

      def head(l: H0 *: T0): H = l.head
      def tail(l: H0 *: T0): T = l.tail
      def cons(h: H0, t: T0): H0 *: T0 = h *: t
    }
}
