package typify
package labelled

import scala.util.chaining.*

trait Remover[T <: Tuple, K] {
  type Out
  def apply(t: T): Out
}

object Remover {
  type Aux[T <: Tuple, K, Out0] = Remover[T, K] { type Out = Out0 }

  inline def apply[T <: Tuple, K](implicit r: Remover[T, K]): Aux[T, K, r.Out] = r
  inline def apply[T <: Tuple, K](t: T, k: K)(implicit r: Remover[T, K]): r.Out = r(t)

  given head[K, V, T <: Tuple]: Aux[(K ->> V) *: T, K, (V, T)] =
    new Remover[(K ->> V) *: T, K] {
      type Out = (V, T)
      def apply(t: (K ->> V) *: T): (V, T) = (t.head, t.tail)
    }

  given tail[H, K, V, TI <: Tuple, TO <: Tuple](using r: Aux[TI, K, (V, TO)]): Aux[H *: TI, K, (V, H *: TO)] =
    new Remover[H *: TI, K] {
      type Out = (V, H *: TO)
      def apply(t: H *: TI): (V, H *: TO) =
        r(t.tail).pipe { case (v, to) => (v, t.head *: to) }
    }
}
