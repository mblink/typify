package typify.tuple

type ZipWithT[T1 <: Tuple, T2 <: Tuple, F[_, _]] <: Tuple = (T1, T2) match {
  case (h1 *: t1, h2 *: t2) => F[h1, h2] *: ZipWithT[t1, t2, F]
  case (EmptyTuple, ?) => EmptyTuple
  case (?, EmptyTuple) => EmptyTuple
  case _ => Tuple
}

/**
 * Type class supporting zipping a `Tuple` with another `Tuple` using a `Poly` resulting in a `Tuple`
 */
trait ZipWith[L, R, F] extends DepFn2[L, R]

object ZipWith {
  type Aux[L, R, F, O] = ZipWith[L, R, F] { type Out = O }

  inline def apply[L, R, F](using z: ZipWith[L, R, F]): ZipWith.Aux[L, R, F, z.Out] = z

  private def constZipWith[L, R, P]: ZipWith.Aux[L, R, P, EmptyTuple] =
    new ZipWith[L, R, P] {
      type Out = EmptyTuple
      def apply(l: L, r: R): EmptyTuple = EmptyTuple
    }

  given emptyTupleZipWithEmptyTuple[F]: ZipWith.Aux[EmptyTuple, EmptyTuple, F, EmptyTuple] = constZipWith[EmptyTuple, EmptyTuple, F]
  given emptyTupleZipWithTuple[R, F]: ZipWith.Aux[EmptyTuple, R, F, EmptyTuple] = constZipWith[EmptyTuple, R, F]
  given tupleZipWithEmptyTuple[L, F]: ZipWith.Aux[L, EmptyTuple, F, EmptyTuple] = constZipWith[L, EmptyTuple, F]

  given tupleZipWithTuple[LH, RH, LT <: Tuple, RT <: Tuple, F, ZipWithOut <: Tuple, ClrResult](
    using zt: ZipWith.Aux[LT, RT, F, ZipWithOut],
    c: Case2.Aux[F, LH, RH, ClrResult],
  ): ZipWith.Aux[LH *: LT, RH *: RT, F, ClrResult *: ZipWithOut] =
    new ZipWith[LH *: LT, RH *: RT, F] {
      type Out = ClrResult *: ZipWithOut
      def apply(l: LH *: LT, r: RH *: RT): Out =
        c(l.head, r.head) *: zt(l.tail, r.tail)
    }
}
