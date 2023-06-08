package typify.tuple

type ZipWith[T1 <: Tuple, T2 <: Tuple, F[_, _]] <: Tuple = (T1, T2) match {
  case (h1 *: t1, h2 *: t2) => F[h1, h2] *: ZipWith[t1, t2, F]
  case (EmptyTuple, ?) => EmptyTuple
  case (?, EmptyTuple) => EmptyTuple
  case _ => Tuple
}
