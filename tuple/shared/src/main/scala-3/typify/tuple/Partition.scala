package typify.tuple

import scala.util.NotGiven

trait Partition[L, U] extends DepFn1[L] {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def filter(l: L): Prefix
  def filterNot(l: L): Suffix

  final def product(l: L): Prefix *: Suffix *: EmptyTuple = filter(l) *: filterNot(l) *: EmptyTuple
  final def apply(l: L): Out = {
    val p = product(l)
    (p.head, p.tail.head)
  }
}

object Partition {
  inline def apply[L, U](using p: Partition[L, U]): Partition.Aux[L, U, p.Prefix, p.Suffix] = p

  type Aux[L, U, P, S] = Partition[L, U] {
    type Prefix = P
    type Suffix = S
  }

  given emptyTuplePartition[U]: Partition.Aux[EmptyTuple, U, EmptyTuple, EmptyTuple] =
    new Partition[EmptyTuple, U] {
      type Prefix = EmptyTuple
      type Suffix = EmptyTuple
      def filter(l: EmptyTuple): EmptyTuple = EmptyTuple
      def filterNot(l: EmptyTuple): EmptyTuple = EmptyTuple
    }

  given tupleNPartition1[H, L <: Tuple, LPrefix <: Tuple, LSuffix <: Tuple](
    using p: Partition.Aux[L, H, LPrefix, LSuffix],
  ): Partition.Aux[H *: L, H, H *: LPrefix, LSuffix] =
    new Partition[H *: L, H] {
      type Prefix = H *: LPrefix
      type Suffix = LSuffix
      def filter(l: H *: L): Prefix = l.head *: p.filter(l.tail)
      def filterNot(l: H *: L): Suffix = p.filterNot(l.tail)
    }

  given tupleNPartition2[H, L <: Tuple, U, LPrefix <: Tuple, LSuffix <: Tuple](
    using ev: NotGiven[U =:= H],
    p: Aux[L, U, LPrefix, LSuffix],
  ): Aux[H *: L, U, LPrefix, H *: LSuffix] =
    new Partition[H *: L, U] {
      type Prefix = LPrefix
      type Suffix = H *: LSuffix
      def filter(l: H *: L): Prefix    = p.filter(l.tail)
      def filterNot(l: H *: L): Suffix = l.head *: p.filterNot(l.tail)
    }
}
