package typify
package labelled

import scala.util.chaining.*

trait Remover[T <: Tuple, K] {
  type Out
  def apply(t: T): Out
}

sealed trait RemoverLP {
  final type Aux[T <: Tuple, K, Out0] = Remover[T, K] { type Out = Out0 }

  final given tailRemover[H, K, V, TI <: Tuple, TO <: Tuple](using r: Aux[TI, K, (V, TO)]): Aux[H *: TI, K, (V, H *: TO)] =
    new Remover[H *: TI, K] {
      type Out = (V, H *: TO)
      def apply(t: H *: TI): (V, H *: TO) =
        r(t.tail).pipe { case (v, to) => (v, t.head *: to) }
    }
}

object Remover extends RemoverLP {
  inline def apply[T <: Tuple, K](implicit r: Remover[T, K]): Aux[T, K, r.Out] = r
  inline def apply[T <: Tuple, K](t: T, k: K)(implicit r: Remover[T, K]): r.Out = r(t)

  /* TODO - revisit specializing Remover instances by generating this code:

  println(0.to(100).map { i =>
    val tps = 0.to(i - 1).map(j => s"A$j")
    val tupleTpe = s"${tps.mkString(" *: ")}${if (tps.isEmpty) "" else " *: "}(K ->> V) *: T"
    val rmTupleTpe = if (tps.isEmpty) "T" else s"${tps.mkString(" *: ")} *: T"
s"""
  given elem${i}Remover[${tps.mkString(", ")}${if (tps.isEmpty) "" else ", "}K, V, T <: Tuple]: Aux[$tupleTpe, K, (V, $rmTupleTpe)] =
    new Remover[$tupleTpe, K] {
      type Out = (V, $rmTupleTpe)
      def apply(t: $tupleTpe): Out = {
        val b = t.toArray.to(collection.mutable.Buffer)
        (b.remove($i).asInstanceOf[V], Tuple.fromArray(b.to(Array)).asInstanceOf[$rmTupleTpe])
      }
    }"""
  }.mkString("\n"))
  */

  given headRemover[K, V, T <: Tuple]: Aux[(K ->> V) *: T, K, (V, T)] =
    new Remover[(K ->> V) *: T, K] {
      type Out = (V, T)
      def apply(t: (K ->> V) *: T): (V, T) = (t.head, t.tail)
    }
}
