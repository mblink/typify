package typify.tuple

trait Reify[T <: Tuple] extends DepFn0 { type Out }

object Reify {
  inline def apply[T <: Tuple](using r: Reify[T]): Aux[T, r.Out] = r

  type Aux[T <: Tuple, Out0] = Reify[T] { type Out = Out0 }

  inline given reifyInst[T <: Tuple]: Aux[T, T] =
    new Reify[T] {
      type Out = T
      def apply(): T =
        Tuple.fromArray(compiletime.summonAll[Tuple.Map[T, ValueOf]]
          .toArray.asInstanceOf[Array[ValueOf[Any]]].map(_.value)).asInstanceOf[T]
    }
}
