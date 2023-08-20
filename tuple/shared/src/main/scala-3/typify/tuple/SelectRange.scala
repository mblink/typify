package typify.tuple

import compiletime.ops.int.>=

/**
 * Type class supporting supporting access to the elements in range [a,b] of this `Tuple`.
 * Available only if this `Tuple` contains all elements in range.
 */
trait SelectRange[L, A, B] extends DepFn1[L]

object SelectRange {
  type Aux[L, A, B, O] = SelectRange[L, A, B] { type Out = O }

  inline def apply[L, A, B](using s: SelectRange[L, A, B]): SelectRange.Aux[L, A, B, s.Out] = s

  given tupleSelectRange[L <: Tuple, A <: Int, B <: Int](
    using a: ValueOf[A],
    b: ValueOf[B],
    ev: (Tuple.Size[L] >= B) =:= true,
  ): SelectRange.Aux[L, A, B, Tuple.Take[Tuple.Drop[L, A], B]] =
    new SelectRange[L, A, B] {
      type Out = Tuple.Take[Tuple.Drop[L, A], B]
      def apply(l: L): Out = l.toArray.drop(a.value).take(b.value).asInstanceOf[Out]
    }
}
