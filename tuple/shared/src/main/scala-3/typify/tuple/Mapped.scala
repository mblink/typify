package typify.tuple

/**
 * Type class witnessing that the result of wrapping each element of `Tuple` `L` in type constructor `F` is `Out`.
 */
trait Mapped[T, F[_]] {
  type Out
}

object Mapped {
  type Aux[T, F[_], O] = Mapped[T, F] { type Out = O }

  inline def apply[T <: Tuple, F[_]](using m: Mapped[T, F]): Mapped.Aux[T, F, m.Out] = m

  given mappedTuple[T <: Tuple, F[_]]: Mapped.Aux[T, F, Tuple.Map[T, F]] =
    new Mapped[T, F] {
      type Out = Tuple.Map[T, F]
    }
}
