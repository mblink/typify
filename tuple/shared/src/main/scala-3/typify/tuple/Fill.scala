package typify.tuple

import compiletime.ops.int.{-, >=}

type FillT[N <: Int, A] <: Tuple = N match {
  case 0 => EmptyTuple
  case _ => A *: FillT[N - 1, A]
}


/**
 * Type class supporting producing a `Tuple` of length `N` filled with elements of type `A`.
 */
trait Fill[N, A] extends DepFn1[A]

object Fill {
  type Aux[N, A, O] = Fill[N, A] { type Out = O }

  inline def apply[N, A](using f: Fill[N, A]): Fill.Aux[N, A, f.Out] = f

  @annotation.tailrec
  private[tuple] def fill[A](i: Int, a: A, acc: Tuple): Tuple =
    if (i <= 0) acc
    else fill(i - 1, a, a *: acc)

  given tupleFill[N <: Int, A](
    using ev: (N >= 0) =:= true,
    n: ValueOf[N],
  ): Fill.Aux[N, A, FillT[N, A]] =
    new Fill[N, A] {
      type Out = FillT[N, A]
      def apply(a: A): Out = fill(n.value, a, EmptyTuple).asInstanceOf[Out]
    }

  given tupleFill2[N <: Int, M <: Int, A](
    using fillM: Fill[M, A],
    fillN: Fill[N, fillM.Out],
  ): Fill.Aux[(N, M), A, fillN.Out] =
    new Fill[(N, M), A] {
      type Out = fillN.Out
      def apply(a: A): Out = fillN(fillM(a))
    }
}
