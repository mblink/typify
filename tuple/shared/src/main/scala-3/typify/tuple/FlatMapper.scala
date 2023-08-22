package typify.tuple

/**
 * Type class supporting flatmapping a higher ranked function of type `F` over this `Tuple`.
 */
trait FlatMapper[F, In <: Tuple] extends DepFn1[In] with Serializable { type Out <: Tuple }

object FlatMapper {
  type Aux[F, In <: Tuple, O <: Tuple] = FlatMapper[F, In] { type Out = O }

  inline def apply[F, L <: Tuple](using m: FlatMapper[F, L]): FlatMapper.Aux[F, L, m.Out] = m

  given flatMapperEmptyTuple[F]: FlatMapper.Aux[F, EmptyTuple, EmptyTuple] =
    new FlatMapper[F, EmptyTuple] {
      type Out = EmptyTuple
      def apply(t: EmptyTuple): Out = EmptyTuple
    }

  given flatMapperTupleN[F <: Poly, InH, InT <: Tuple, OutH <: Tuple, OutT <: Tuple, Out0 <: Tuple](
    using hc: Case1.Aux[F, InH, OutH],
    mt: FlatMapper.Aux[F, InT, OutT],
    p: Prepend.Aux[OutH, OutT, Out0],
  ): FlatMapper.Aux[F, InH *: InT, Out0] =
    new FlatMapper[F, InH *: InT] {
      type Out = Out0
      def apply(t: InH *: InT): Out = p(hc(t.head), mt(t.tail))
    }
}
