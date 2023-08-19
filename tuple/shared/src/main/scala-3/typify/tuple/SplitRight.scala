package typify.tuple

trait SplitRight[L, U] extends DepFn1[L] {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def product(l: L): Prefix *: Suffix *: EmptyTuple

  final def apply(l: L): Out = {
    val p = product(l)
    (p.head, p.tail.head)
  }
}

object SplitRight {
  type Aux[L, U, P, S] = SplitRight[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: SplitRight[T, U]): SplitRight.Aux[T, U, s.Prefix, s.Suffix] = s

  given splitRightTuple[L <: Tuple, U](
    using idxv: ValueOf[ElemIndex[Reverse[L], U]],
  ): SplitRight.Aux[L, U, Reverse[Tuple.Drop[Reverse[L], ElemIndex[Reverse[L], U]]], Reverse[Tuple.Take[Reverse[L], ElemIndex[Reverse[L], U]]]] =
    new SplitRight[L, U] {
      type Prefix = Reverse[Tuple.Drop[Reverse[L], ElemIndex[Reverse[L], U]]]
      type Suffix = Reverse[Tuple.Take[Reverse[L], ElemIndex[Reverse[L], U]]]

      private lazy val n = idxv.value
      def product(l: L): Prefix *: Suffix *: EmptyTuple = {
        val a = l.toArray.reverse
        Tuple.fromArray(a.drop(n).reverse).asInstanceOf[Prefix] *:
          Tuple.fromArray(a.take(n).reverse).asInstanceOf[Suffix] *:
          EmptyTuple
      }
    }
}
