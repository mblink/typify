package typify.tuple

type ToEitherT[T <: Tuple] = T match {
  case EmptyTuple => Nothing
  case l *: EmptyTuple => Either[l, Nothing]
  case l *: r *: EmptyTuple => Either[l, r]
  case l *: r => Either[l, ToEitherT[r]]
}

/**
 * Type class computing the `Either` type corresponding to this `Tuple`.
 */
trait ToEither[T] extends Serializable {
  type Out
}

object ToEither {
  type Aux[T, O] = ToEither[T] { type Out = O }

  inline def apply[T <: Tuple](using e: ToEither[T]): ToEither.Aux[T, e.Out] = e

  given toEitherNonEmptyTuple[T <: Tuple]: ToEither.Aux[T, ToEitherT[T]] =
    new ToEither[T] {
      type Out = ToEitherT[T]
    }
}
