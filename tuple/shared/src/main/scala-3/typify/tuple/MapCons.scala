package typify.tuple

type MapConsT[A, M <: Tuple] <: Tuple = M match {
  case EmptyTuple => EmptyTuple
  case h *: t => (A *: TupleType[h]) *: MapConsT[A, t]
}

/**
 * Type class supporting consing an element onto each row of this `Tuple` of `Tuple`s.
 */
trait MapCons[A, M] extends DepFn2[A, M] with Serializable

object MapCons {
  type Aux[A, M, O] = MapCons[A, M] { type Out = O }

  inline def apply[A, M](using m: MapCons[A, M]): MapCons.Aux[A, M, m.Out] = m

  given tupleMapCons[A, M <: Tuple](
    using ev: LiftAll[[a] =>> a <:< Tuple, M],
  ): MapCons.Aux[A, M, MapConsT[A, M]] =
    new MapCons[A, M] {
      type Out = MapConsT[A, M]
      def apply(a: A, m: M): Out =
        Tuple.fromArray(m.toArray.map[Tuple](x =>
          Tuple.fromArray(a.asInstanceOf[Object] +: x.asInstanceOf[Tuple].toArray)
        )).asInstanceOf[Out]
    }
}
