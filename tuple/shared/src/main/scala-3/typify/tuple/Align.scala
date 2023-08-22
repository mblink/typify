package typify.tuple

/**
 * Type class supporting permuting this `Tuple` into the same order as another `Tuple` with the same element types.
 */
trait Align[L, M] extends (L => M) with Serializable

object Align {
  inline def apply[L, M](using a: Align[L, M]): Align[L, M] = a

  given emptyTupleAlign: Align[EmptyTuple, EmptyTuple] =
    new Align[EmptyTuple, EmptyTuple] {
      def apply(l: EmptyTuple): EmptyTuple = l
    }

  given tupleNAlign[L <: Tuple, MH, MT <: Tuple, R <: Tuple](
    using r: Remove.Aux[L, MH, (MH, R)],
    at: Align[R, MT],
  ): Align[L, MH *: MT] =
    new Align[L, MH *: MT] {
      def apply(l: L): MH *: MT = {
        val (h, t) = r(l)
        h *: at(t)
      }
    }
}
