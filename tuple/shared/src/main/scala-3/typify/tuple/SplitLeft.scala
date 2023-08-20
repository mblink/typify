package typify.tuple

/**
 * Type class supporting splitting this `Tuple` at the first occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `Tuple` contains an element of type `U`.
 */
trait SplitLeft[L, U] extends DepFn1[L] {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def product(l: L): Prefix *: Suffix *: EmptyTuple

  final def apply(l: L): Out = {
    val p = product(l)
    (p.head, p.tail.head)
  }
}

object SplitLeft {
  type Aux[L, U, P, S] = SplitLeft[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: SplitLeft[T, U]): SplitLeft.Aux[T, U, s.Prefix, s.Suffix] = s

  given splitLeftTuple[L <: Tuple, U](
    using idxv: ValueOf[ElemIndex[L, U]],
  ): SplitLeft.Aux[L, U, Tuple.Take[L, ElemIndex[L, U]], Tuple.Drop[L, ElemIndex[L, U]]] =
    new SplitLeft[L, U] {
      type Prefix = Tuple.Take[L, ElemIndex[L, U]]
      type Suffix = Tuple.Drop[L, ElemIndex[L, U]]
      private lazy val n = idxv.value
      def product(l: L): Prefix *: Suffix *: EmptyTuple =
        l.take(n).asInstanceOf[Prefix] *: l.drop(n).asInstanceOf[Suffix] *: EmptyTuple
    }
}
