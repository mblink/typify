package typify.tuple

/**
 * Type class supporting splitting this `Tuple` at the last occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `Tuple` contains an element of type `U`.
 */
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
    using idxv: ValueOf[ElemIndex[ReverseT[L], U]],
  ): SplitRight.Aux[L, U, ReverseT[Tuple.Drop[ReverseT[L], ElemIndex[ReverseT[L], U]]], ReverseT[Tuple.Take[ReverseT[L], ElemIndex[ReverseT[L], U]]]] =
    new SplitRight[L, U] {
      type Prefix = ReverseT[Tuple.Drop[ReverseT[L], ElemIndex[ReverseT[L], U]]]
      type Suffix = ReverseT[Tuple.Take[ReverseT[L], ElemIndex[ReverseT[L], U]]]

      private lazy val n = idxv.value
      def product(l: L): Prefix *: Suffix *: EmptyTuple = {
        val a = l.toArray.reverse
        Tuple.fromArray(a.drop(n).reverse).asInstanceOf[Prefix] *:
          Tuple.fromArray(a.take(n).reverse).asInstanceOf[Suffix] *:
          EmptyTuple
      }
    }
}
