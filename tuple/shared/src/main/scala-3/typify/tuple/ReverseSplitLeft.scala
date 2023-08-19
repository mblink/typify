package typify.tuple

trait ReverseSplitLeft[L, U] extends DepFn1[L] {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def product(l: L): Prefix *: Suffix *: EmptyTuple

  final def apply(l: L): Out = {
    val p = product(l)
    (p.head, p.tail.head)
  }
}

object ReverseSplitLeft {
  type Aux[L, U, P, S] = ReverseSplitLeft[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: ReverseSplitLeft[T, U]): ReverseSplitLeft.Aux[T, U, s.Prefix, s.Suffix] = s

  given reverseSplitLeftTuple[L <: Tuple, U](
    using idxv: ValueOf[ElemIndex[L, U]],
  ): ReverseSplitLeft.Aux[L, U, ReverseT[Tuple.Take[L, ElemIndex[L, U]]], Tuple.Drop[L, ElemIndex[L, U]]] =
    new ReverseSplitLeft[L, U] {
      type Prefix = ReverseT[Tuple.Take[L, ElemIndex[L, U]]]
      type Suffix = Tuple.Drop[L, ElemIndex[L, U]]
      private lazy val n = idxv.value
      def product(l: L): Prefix *: Suffix *: EmptyTuple =
        l.take(n).reverse.asInstanceOf[Prefix] *: l.drop(n).asInstanceOf[Suffix] *: EmptyTuple
    }
}
