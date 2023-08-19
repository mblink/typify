package typify.tuple

trait Reify[T <: Tuple] extends DepFn0 { type Out }

object Reify {
  inline def apply[T <: Tuple](using r: Reify[T]): Reify.Aux[T, r.Out] = r

  type Aux[T <: Tuple, Out0] = Reify[T] { type Out = Out0 }

  inline given reifyInst[T <: Tuple]: Reify.Aux[T, T] =
    new Reify[T] {
      type Out = T
      def apply(): T =
        compiletime.summonAll[Tuple.Map[T, ValueOf]].toList
          .foldRight[Tuple](EmptyTuple)((v, acc) => v.asInstanceOf[ValueOf[_]].value *: acc)
          .asInstanceOf[T]
    }
}
