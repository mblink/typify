package typify.record

import typify.tuple.DepFn1

/**
 * Type class supporting removal and re-insertion of an element (possibly unlabelled).
 */
trait Remove[L, E] extends DepFn1[L] {
  def reinsert(out: Out): L
}

sealed trait RemoveLP {
  final type Aux[L, E, O] = Remove[L, E] { type Out = O }

  final given tupleNRemove[H, T <: Tuple, E, OutT <: Tuple](
    using rt: Remove.Aux[T, E, (E, OutT)],
  ): Remove.Aux[H *: T, E, (E, H *: OutT)] =
    new Remove[H *: T, E] {
      type Out = (E, H *: OutT)
      def apply(l: H *: T): Out = {
        val (e, tail) = rt(l.tail)
        (e, l.head *: tail)
      }
      def reinsert(out: Out): H *: T = out._2.head *: rt.reinsert((out._1, out._2.tail))
    }
}

object Remove extends RemoveLP {
  inline def apply[L, E](using r: Remove[L, E]): Remove.Aux[L, E, r.Out] = r

  given removeHead[K, V, T <: Tuple]: Remove.Aux[(K ->> V) *: T, (K ->> V), ((K ->> V), T)] =
    new Remove[(K ->> V) *: T, (K ->> V)] {
      type Out = (K ->> V, T)
      def apply(l: (K ->> V) *: T): Out = (l.head, l.tail)
      def reinsert(out: Out): (K ->> V) *: T = out._1 *: out._2
    }

  given removeUnlabelledHead[K, V, T <: Tuple]: Remove.Aux[(K ->> V) *: T, V, (V, T)] =
    new Remove[(K ->> V) *: T, V] {
      type Out = (V, T)
      def apply(l: (K ->> V) *: T): Out = (l.head, l.tail)
      def reinsert(out: Out): (K ->> V) *: T = label[K](out._1) *: out._2
    }
}
