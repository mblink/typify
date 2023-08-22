package typify.tuple

/**
 * Type class supporting access to the all elements of this `Tuple` of type `U`.
 */
trait Filter[L, U] extends DepFn1[L]

object Filter {
  type Aux[L, U, O] = Filter[L, U] { type Out = O }

  inline def apply[L, U](using f: Filter[L, U]): Filter.Aux[L, U, f.Out] = f

  given filterByPartition[L, U, Prefix, Suffix](
    using p: Partition.Aux[L, U, Prefix, Suffix]
  ): Filter.Aux[L, U, Prefix] =
    new Filter[L, U] {
      type Out = Prefix
      def apply(l: L): Out = p.filter(l)
    }
}
