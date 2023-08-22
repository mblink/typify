package typify.tuple

/**
 * Type class supporting mapping a higher ranked function over this `Tuple`.
 */
trait Mapper[F, In <: Tuple] extends DepFn1[In] with Serializable { type Out <: Tuple }

object Mapper {
  type Aux[F, In <: Tuple, O <: Tuple] = Mapper[F, In] { type Out = O }

  inline def apply[F, L <: Tuple](using m: Mapper[F, L]): Mapper.Aux[F, L, m.Out] = m

  given mapperEmptyTuple[F]: Mapper.Aux[F, EmptyTuple, EmptyTuple] =
    new Mapper[F, EmptyTuple] {
      type Out = EmptyTuple
      def apply(l: EmptyTuple): Out = EmptyTuple
    }

  given mapperTupleCons[F <: Poly, InH, InT <: Tuple, OutH, OutT <: Tuple](
    using hc: Case1.Aux[F, InH, OutH],
    mt: Mapper.Aux[F, InT, OutT],
  ): Mapper.Aux[F, InH *: InT, OutH *: OutT] =
    new Mapper[F, InH *: InT] {
      type Out = OutH *: OutT
      def apply(l: InH *: InT): Out = hc(l.head) *: mt(l.tail)
    }
}
