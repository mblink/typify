package typify.record

import scala.util.NotGiven

trait Extractor[L, E] extends (L => E)

trait ExtractorLP {
  given extract[L <: Tuple, K, V, ET <: Tuple, V1, LR <: Tuple](
    using ev0: NotGiven[L =:= ((K ->> V) *: ET)],
    r: Remover.Aux[L, K, (V1, LR)],
    ev: V1 <:< V,
    ds: Extractor[LR, ET]
  ): Extractor[L, (K ->> V) *: ET] =
  new Extractor[L, (K ->> V) *: ET] {
    def apply(c: L): (K ->> V) *: ET = {
      val (h, t) = r(c)
      label[K](ev(h)) *: ds(t)
    }
  }
}

object Extractor extends ExtractorLP {
  inline def apply[L, E](using e: Extractor[L, E]): Extractor[L, E] = e

  given hnil[L <: Tuple, E <: Tuple](using ev: EmptyTuple =:= E): Extractor[L, E] =
    new Extractor[L, E] {
      def apply(c: L): E = EmptyTuple
    }

  private val identicalInst: Extractor[Tuple, Tuple] = t => t

  given identical[L <: Tuple]: Extractor[L, L] = identicalInst.asInstanceOf[Extractor[L, L]]

  given descend[L <: Tuple, K, V <: Tuple, V1 <: Tuple, LR <: Tuple, ET <: Tuple](
    using ev0: NotGiven[L =:= ((K ->> V) *: ET)],
    r: Remover.Aux[L, K, (V1, LR)],
    ds1: Extractor[V1, V],
    ds2: Extractor[LR, ET]
  ): Extractor[L, (K ->> V) *: ET] =
    new Extractor[L, (K ->> V) *: ET] {
      def apply(c: L): (K ->> V) *: ET = {
        val (h, t) = r(c)
        label[K](ds1(h)) *: ds2(t)
      }
    }
}
