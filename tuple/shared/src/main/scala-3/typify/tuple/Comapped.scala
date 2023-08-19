package typify.tuple

trait Comapped[T, F[_]] {
  type Out
}

object Comapped {
  type Aux[T, F[_], O] = Comapped[T, F] { type Out = O }

  inline def apply[T <: Tuple, F[_]](using m: Comapped[T, F]): Comapped.Aux[T, F, m.Out] = m

  given comappedTuple[T <: Tuple, F[_]](using ev: Tuple.IsMappedBy[F][T]): Comapped.Aux[T, F, Tuple.InverseMap[T, F]] =
    new Comapped[T, F] {
      type Out = Tuple.InverseMap[T, F]
    }
}
