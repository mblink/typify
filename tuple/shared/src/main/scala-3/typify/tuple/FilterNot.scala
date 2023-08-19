package typify.tuple

trait FilterNot[L, U] extends DepFn1[L]

object FilterNot {
  type Aux[L, U, O] = FilterNot[L, U] { type Out = O }

  inline def apply[L, U](implicit f: FilterNot[L, U]): FilterNot.Aux[L, U, f.Out] = f

  given filterByPartition[L, U, Prefix, Suffix](
    using p: Partition.Aux[L, U, Prefix, Suffix]
  ): FilterNot.Aux[L, U, Suffix] =
    new FilterNot[L, U] {
      type Out = Suffix
      def apply(l: L): Out = p.filterNot(l)
    }
}
