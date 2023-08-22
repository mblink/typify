package typify.tuple

/**
 * Type class supporting access to the all elements of this `Tuple` of type different than `U`.
 */
trait FilterNot[L, U] extends DepFn1[L]

object FilterNot {
  type Aux[L, U, O] = FilterNot[L, U] { type Out = O }

  inline def apply[L, U](using f: FilterNot[L, U]): FilterNot.Aux[L, U, f.Out] = f

  given filterByPartition[L, U, Prefix, Suffix](
    using p: Partition.Aux[L, U, Prefix, Suffix]
  ): FilterNot.Aux[L, U, Suffix] =
    new FilterNot[L, U] {
      type Out = Suffix
      def apply(l: L): Out = p.filterNot(l)
    }
}
