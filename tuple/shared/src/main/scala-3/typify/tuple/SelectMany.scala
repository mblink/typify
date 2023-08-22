package typify.tuple

type SelectManyT[L <: Tuple, Ids <: Tuple] <: Tuple = Ids match {
  case h *: t => Tuple.Elem[L, h] *: SelectManyT[L, t]
  case EmptyTuple => EmptyTuple
}

/**
 * Type class supporting access to the elements of this `Tuple` specified by `Ids`. Available only if this `Tuple`
 * contains all elements specified in `Ids`.
 */
trait SelectMany[L, Ids] extends DepFn1[L]

object SelectMany {
  type Aux[L, Ids, O] = SelectMany[L, Ids] { type Out = O }

  inline def apply[L, Ids](using s: SelectMany[L, Ids]): SelectMany.Aux[L, Ids, s.Out] = s

  inline given tupleSelectMany[L <: Tuple, Ids <: Tuple]: SelectMany.Aux[L, Ids, SelectManyT[L, Ids]] =
    new SelectMany[L, Ids] {
      type Out = SelectManyT[L, Ids]
      def apply(l: L): Out = {
        lazy val la = l.toArray
        Tuple.fromArray(
          compiletime.summonAll[Tuple.Map[Ids, ValueOf]].toArray.map(i => la(i.asInstanceOf[ValueOf[Int]].value))
        ).asInstanceOf[Out]
      }
    }
}
