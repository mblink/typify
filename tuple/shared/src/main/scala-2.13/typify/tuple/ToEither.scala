package typify.tuple

trait ToEither[T] extends Serializable {
  type Out
}

sealed trait ToEitherLP {
  final type Aux[T, O] = ToEither[T] { type Out = O }

  final implicit def toEitherTupleN[L, R <: Tuple](implicit r: ToEither[R]): ToEither.Aux[L *: R, Either[L, r.Out]] =
    new ToEither[L *: R] {
      type Out = Either[L, r.Out]
    }
}

object ToEither extends ToEitherLP {
  def apply[T <: Tuple](implicit e: ToEither[T]): ToEither.Aux[T, e.Out] = e

  implicit val toEitherEmptyTuple: ToEither.Aux[EmptyTuple, Nothing] =
    new ToEither[EmptyTuple] {
      type Out = Nothing
    }

  implicit def toEitherTuple1[L]: ToEither.Aux[L *: EmptyTuple, Either[L, Nothing]] =
    new ToEither[L *: EmptyTuple] {
      type Out = Either[L, Nothing]
    }

  implicit def toEitherTuple2[L, R]: ToEither.Aux[L *: R *: EmptyTuple, Either[L, R]] =
    new ToEither[L *: R *: EmptyTuple] {
      type Out = Either[L, R]
    }
}
