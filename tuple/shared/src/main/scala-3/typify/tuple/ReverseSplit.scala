package typify.tuple

import compiletime.ops.int.>=

trait ReverseSplit[L, N] extends DepFn1[L] {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def product(l: L): Prefix *: Suffix *: EmptyTuple

  final def apply(l: L): Out = {
    val p = product(l)
    (p.head, p.tail.head)
  }
}

object ReverseSplit {
  type Aux[L, N, P, S] = ReverseSplit[L, N] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, N](using s: ReverseSplit[T, N]): ReverseSplit.Aux[T, N, s.Prefix, s.Suffix] = s

  given splitTuple[L <: Tuple, N <: Int](
    using ev: (Tuple.Size[L] >= N) =:= true,
    nv: ValueOf[N],
  ): ReverseSplit.Aux[L, N, ReverseT[Tuple.Take[L, N]], Tuple.Drop[L, N]] =
    new ReverseSplit[L, N] {
      type Prefix = ReverseT[Tuple.Take[L, N]]
      type Suffix = Tuple.Drop[L, N]
      private lazy val n = nv.value
      def product(l: L): Prefix *: Suffix *: EmptyTuple =
        l.take(n).reverse.asInstanceOf[Prefix] *: l.drop(n).asInstanceOf[Suffix] *: EmptyTuple
    }
}
