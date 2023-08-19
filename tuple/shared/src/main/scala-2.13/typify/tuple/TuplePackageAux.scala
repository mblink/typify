package typify.tuple

import scala.language.implicitConversions

final class TypifyTupleOps[T <: Tuple](private val t: T) extends AnyVal {
  final def *:[H](h: H): shapeless.::[H, T] = new shapeless.::(h, t)
  final def mapPoly(f: Poly)(implicit m: Mapper[f.type, T]): m.Out = m(t)
  final def toList[Lub](implicit tl: ToList[T, Lub]): List[Lub] = tl(t)
}

trait TuplePackageAux {
  final type Tuple = shapeless.HList
  final type *:[H, T <: Tuple] = shapeless.::[H, T]
  final val *: : shapeless.::.type = shapeless.::
  final type EmptyTuple = shapeless.HNil
  final val EmptyTuple: EmptyTuple = shapeless.HNil

  @inline final implicit def toTypifyTupleOps[T <: Tuple](t: T): TypifyTupleOps[T] = new TypifyTupleOps[T](t)

  final type Align[L <: Tuple, M <: Tuple] = shapeless.ops.hlist.Align[L, M]
  final val Align: shapeless.ops.hlist.Align.type = shapeless.ops.hlist.Align

  final type Collect[I <: Tuple, P <: shapeless.Poly] = shapeless.ops.hlist.Collect[I, P]
  final val Collect: shapeless.ops.hlist.Collect.type = shapeless.ops.hlist.Collect

  final type Comapped[T <: Tuple, F[_]] = shapeless.ops.hlist.Comapped[T, F]
  final val Comapped: shapeless.ops.hlist.Comapped.type = shapeless.ops.hlist.Comapped

  final type ConstMapper[C, T <: Tuple] = shapeless.ops.hlist.ConstMapper[C, T]
  final val ConstMapper: shapeless.ops.hlist.ConstMapper.type = shapeless.ops.hlist.ConstMapper

  final type Drop[T <: Tuple, N <: shapeless.Nat] = shapeless.ops.hlist.Drop[T, N]
  final val Drop: shapeless.ops.hlist.Drop.type = shapeless.ops.hlist.Drop

  final type Generic[A] = shapeless.Generic[A]
  final val Generic: shapeless.Generic.type = shapeless.Generic

  final type FillWith[F, L <: Tuple] = shapeless.ops.hlist.FillWith[F, L]
  final val FillWith: shapeless.ops.hlist.FillWith.type = shapeless.ops.hlist.FillWith

  final type Filter[L <: Tuple, U] = shapeless.ops.hlist.Filter[L, U]
  final val Filter: shapeless.ops.hlist.Filter.type = shapeless.ops.hlist.Filter

  final type FilterNot[L <: Tuple, U] = shapeless.ops.hlist.FilterNot[L, U]
  final val FilterNot: shapeless.ops.hlist.FilterNot.type = shapeless.ops.hlist.FilterNot

  final type FlatMapper[F, In <: Tuple] = shapeless.ops.hlist.FlatMapper[F, In]
  final val FlatMapper: shapeless.ops.hlist.FlatMapper.type = shapeless.ops.hlist.FlatMapper

  final type FlatMapInterleave[A, L <: Tuple] = shapeless.ops.hlist.FlatMapInterleave[A, L]
  final val FlatMapInterleave: shapeless.ops.hlist.FlatMapInterleave.type = shapeless.ops.hlist.FlatMapInterleave

  final type Grouper[L <: Tuple, N <: shapeless.Nat, Step <: shapeless.Nat] = shapeless.ops.hlist.Grouper[L, N, Step]
  final val Grouper: shapeless.ops.hlist.Grouper.type = shapeless.ops.hlist.Grouper

  final type Init[T <: Tuple] = shapeless.ops.hlist.Init[T]
  final val Init: shapeless.ops.hlist.Init.type = shapeless.ops.hlist.Init

  final type Interleave[A, L <: Tuple] = shapeless.ops.hlist.Interleave[A, L]
  final val Interleave: shapeless.ops.hlist.Interleave.type = shapeless.ops.hlist.Interleave

  final type IsNonEmptyTuple[T <: Tuple] = shapeless.ops.hlist.IsHCons[T]
  final val IsNonEmptyTuple: shapeless.ops.hlist.IsHCons.type = shapeless.ops.hlist.IsHCons

  final type Last[T <: Tuple] = shapeless.ops.hlist.Last[T]
  final val Last: shapeless.ops.hlist.Last.type = shapeless.ops.hlist.Last

  final type LeftFolder[L <: Tuple, In, F] = shapeless.ops.hlist.LeftFolder[L, In, F]
  final val LeftFolder: shapeless.ops.hlist.LeftFolder.type = shapeless.ops.hlist.LeftFolder

  final type LeftReducer[L <: Tuple, F] = shapeless.ops.hlist.LeftReducer[L, F]
  final val LeftReducer: shapeless.ops.hlist.LeftReducer.type = shapeless.ops.hlist.LeftReducer

  final type Length[T <: Tuple] = shapeless.ops.hlist.Length[T]
  final val Length: shapeless.ops.hlist.Length.type = shapeless.ops.hlist.Length

  final type LiftAll[F[_], In <: Tuple] = shapeless.ops.hlist.LiftAll[F, In]
  final val LiftAll: shapeless.ops.hlist.LiftAll.type = shapeless.ops.hlist.LiftAll

  final type MapCons[A, M <: Tuple] = shapeless.ops.hlist.MapCons[A, M]
  final val MapCons: shapeless.ops.hlist.MapCons.type = shapeless.ops.hlist.MapCons

  final type MapFolder[T <: Tuple, R, F] = shapeless.ops.hlist.MapFolder[T, R, F]
  final val MapFolder: shapeless.ops.hlist.MapFolder.type = shapeless.ops.hlist.MapFolder

  final type Mapped[T <: Tuple, F[_]] = shapeless.ops.hlist.Mapped[T, F]
  final val Mapped: shapeless.ops.hlist.Mapped.type = shapeless.ops.hlist.Mapped

  final type Mapper[F, In <: Tuple] = shapeless.ops.hlist.Mapper[F, In]
  final val Mapper: shapeless.ops.hlist.Mapper.type = shapeless.ops.hlist.Mapper

  final type Modifier[L <: Tuple, U, V] = shapeless.ops.hlist.Modifier[L, U, V]
  final val Modifier: shapeless.ops.hlist.Modifier.type = shapeless.ops.hlist.Modifier

  final type ModifierAt[L <: Tuple, N <: shapeless.Nat, U, V] = shapeless.ops.hlist.ModifierAt[L, N, U, V]
  final val ModifierAt: shapeless.ops.hlist.ModifierAt.type = shapeless.ops.hlist.ModifierAt

  final type Partition[L <: Tuple, U] = shapeless.ops.hlist.Partition[L, U]
  final val Partition: shapeless.ops.hlist.Partition.type = shapeless.ops.hlist.Partition

  final type Permutations[T <: Tuple] = shapeless.ops.hlist.Permutations[T]
  final val Permutations: shapeless.ops.hlist.Permutations.type = shapeless.ops.hlist.Permutations

  final type Prepend[L <: Tuple, R <: Tuple] = shapeless.ops.hlist.Prepend[L, R]
  final val Prepend: shapeless.ops.hlist.Prepend.type = shapeless.ops.hlist.Prepend

  final type Reify[T <: Tuple] = shapeless.ops.hlist.Reify[T]
  final val Reify: shapeless.ops.hlist.Reify.type = shapeless.ops.hlist.Reify

  final type Remove[L <: Tuple, U] = shapeless.ops.hlist.Remove[L, U]
  final val Remove: shapeless.ops.hlist.Remove.type = shapeless.ops.hlist.Remove

  final type RemoveAll[L <: Tuple, SL <: Tuple] = shapeless.ops.hlist.RemoveAll[L, SL]
  final val RemoveAll: shapeless.ops.hlist.RemoveAll.type = shapeless.ops.hlist.RemoveAll

  final type Repeat[L <: Tuple, N <: shapeless.Nat] = shapeless.ops.hlist.Repeat[L, N]
  final val Repeat: shapeless.ops.hlist.Repeat.type = shapeless.ops.hlist.Repeat

  final type Replacer[L <: Tuple, U, V] = shapeless.ops.hlist.Replacer[L, U, V]
  final val Replacer: shapeless.ops.hlist.Replacer.type = shapeless.ops.hlist.Replacer

  final type ReplaceAt[L <: Tuple, N <: shapeless.Nat, V] = shapeless.ops.hlist.ReplaceAt[L, N, V]
  final val ReplaceAt: shapeless.ops.hlist.ReplaceAt.type = shapeless.ops.hlist.ReplaceAt

  final type Reverse[T <: Tuple] = shapeless.ops.hlist.Reverse[T]
  final val Reverse: shapeless.ops.hlist.Reverse.type = shapeless.ops.hlist.Reverse

  final type ReversePrepend[L <: Tuple, R <: Tuple] = shapeless.ops.hlist.ReversePrepend[L, R]
  final val ReversePrepend: shapeless.ops.hlist.ReversePrepend.type = shapeless.ops.hlist.ReversePrepend

  final type ReverseSplit[T <: Tuple, N <: shapeless.Nat] = shapeless.ops.hlist.ReverseSplit[T, N]
  final val ReverseSplit: shapeless.ops.hlist.ReverseSplit.type = shapeless.ops.hlist.ReverseSplit

  final type ReverseSplitLeft[T <: Tuple, U] = shapeless.ops.hlist.ReverseSplitLeft[T, U]
  final val ReverseSplitLeft: shapeless.ops.hlist.ReverseSplitLeft.type = shapeless.ops.hlist.ReverseSplitLeft

  final type ReverseSplitRight[T <: Tuple, U] = shapeless.ops.hlist.ReverseSplitRight[T, U]
  final val ReverseSplitRight: shapeless.ops.hlist.ReverseSplitRight.type = shapeless.ops.hlist.ReverseSplitRight

  final type RightFolder[L <: Tuple, In, F] = shapeless.ops.hlist.RightFolder[L, In, F]
  final val RightFolder: shapeless.ops.hlist.RightFolder.type = shapeless.ops.hlist.RightFolder

  final type RightReducer[L <: Tuple, F] = shapeless.ops.hlist.RightReducer[L, F]
  final val RightReducer: shapeless.ops.hlist.RightReducer.type = shapeless.ops.hlist.RightReducer

  final type RotateLeft[L <: Tuple, N <: shapeless.Nat] = shapeless.ops.hlist.RotateLeft[L, N]
  final val RotateLeft: shapeless.ops.hlist.RotateLeft.type = shapeless.ops.hlist.RotateLeft

  final type RotateRight[L <: Tuple, N <: shapeless.Nat] = shapeless.ops.hlist.RotateRight[L, N]
  final val RotateRight: shapeless.ops.hlist.RotateRight.type = shapeless.ops.hlist.RotateRight

  final type SelectAll[L <: Tuple, S <: Tuple] = shapeless.ops.hlist.SelectAll[L, S]
  final val SelectAll: shapeless.ops.hlist.SelectAll.type = shapeless.ops.hlist.SelectAll

  final type SelectMany[L <: Tuple, Ids <: Tuple] = shapeless.ops.hlist.SelectMany[L, Ids]
  final val SelectMany: shapeless.ops.hlist.SelectMany.type = shapeless.ops.hlist.SelectMany

  final type SelectRange[L <: Tuple, A <: shapeless.Nat, B <: shapeless.Nat] = shapeless.ops.hlist.SelectRange[L, A, B]
  final val SelectRange: shapeless.ops.hlist.SelectRange.type = shapeless.ops.hlist.SelectRange

  final type Split[T <: Tuple, N <: shapeless.Nat] = shapeless.ops.hlist.Split[T, N]
  final val Split: shapeless.ops.hlist.Split.type = shapeless.ops.hlist.Split

  final type SplitLeft[T <: Tuple, U] = shapeless.ops.hlist.SplitLeft[T, U]
  final val SplitLeft: shapeless.ops.hlist.SplitLeft.type = shapeless.ops.hlist.SplitLeft

  final type SplitRight[T <: Tuple, U] = shapeless.ops.hlist.SplitRight[T, U]
  final val SplitRight: shapeless.ops.hlist.SplitRight.type = shapeless.ops.hlist.SplitRight

  final type SubtypeUnifier[T <: Tuple, B] = shapeless.ops.hlist.SubtypeUnifier[T, B]
  final val SubtypeUnifier: shapeless.ops.hlist.SubtypeUnifier.type = shapeless.ops.hlist.SubtypeUnifier

  final type Take[T <: Tuple, N <: shapeless.Nat] = shapeless.ops.hlist.Take[T, N]
  final val Take: shapeless.ops.hlist.Take.type = shapeless.ops.hlist.Take

  final type ToArray[L <: Tuple, Lub] = ToTraversable.Aux[L, Array, Lub]
  final def ToArray[L <: Tuple, Lub](implicit t: ToArray[L, Lub]): ToArray[L, Lub] = t

  final type ToList[L <: Tuple, Lub] = ToTraversable.Aux[L, List, Lub]
  final def ToList[L <: Tuple, Lub](implicit t: ToList[L, Lub]): ToList[L, Lub] = t

  final type ToTraversable[L <: Tuple, M[_]] = shapeless.ops.hlist.ToTraversable[L, M]
  final val ToTraversable: shapeless.ops.hlist.ToTraversable.type = shapeless.ops.hlist.ToTraversable

  final type Transposer[L <: Tuple] = shapeless.ops.hlist.Transposer[L]
  final val Transposer: shapeless.ops.hlist.Transposer.type = shapeless.ops.hlist.Transposer

  final type TupleSelector[T <: Tuple, A] = shapeless.ops.hlist.Selector[T, A]
  final val TupleSelector: shapeless.ops.hlist.Selector.type = shapeless.ops.hlist.Selector

  final type Unifier[T <: Tuple] = shapeless.ops.hlist.Unifier[T]
  final val Unifier: shapeless.ops.hlist.Unifier.type = shapeless.ops.hlist.Unifier

  final type ZipApply[FL <: Tuple, AL <: Tuple] = shapeless.ops.hlist.ZipApply[FL, AL]
  final val ZipApply: shapeless.ops.hlist.ZipApply.type = shapeless.ops.hlist.ZipApply

  final type ZipConst[C, T <: Tuple] = shapeless.ops.hlist.ZipConst[C, T]
  final val ZipConst: shapeless.ops.hlist.ZipConst.type = shapeless.ops.hlist.ZipConst

  final type ZipOne[H <: Tuple, T <: Tuple] = shapeless.ops.hlist.ZipOne[H, T]
  final val ZipOne: shapeless.ops.hlist.ZipOne.type = shapeless.ops.hlist.ZipOne

  final type ZipWith[L <: Tuple, R <: Tuple, F <: shapeless.Poly2] = shapeless.ops.hlist.ZipWith[L, R, F]
  final val ZipWith: shapeless.ops.hlist.ZipWith.type = shapeless.ops.hlist.ZipWith

  final type ZipWithIndex[L <: Tuple] = shapeless.ops.hlist.ZipWithIndex[L]
  final val ZipWithIndex: shapeless.ops.hlist.ZipWithIndex.type = shapeless.ops.hlist.ZipWithIndex

  final type ZipWithKeys[K <: Tuple, V <: Tuple] = shapeless.ops.hlist.ZipWithKeys[K, V]
  final val ZipWithKeys: shapeless.ops.hlist.ZipWithKeys.type = shapeless.ops.hlist.ZipWithKeys

  final type DepFn0 = shapeless.DepFn0
  final type DepFn1[T] = shapeless.DepFn1[T]
  final type DepFn2[T, U] = shapeless.DepFn2[T, U]
  final type Poly = shapeless.Poly

  trait Poly0 extends shapeless.Poly {
    final type Case[A] = shapeless.PolyDefns.Case0.Aux[this.type, A]
    @inline final def at[A](f: () => A): Case[A] =
      new shapeless.PolyDefns.Case0[this.type] {
        type Result = A
        val value = (_: EmptyTuple) => f()
      }
  }
  trait Poly1 extends shapeless.Poly {
    final type Case[A, B] = shapeless.PolyDefns.Case1.Aux[this.type, A, B]
    @inline final def at[A, B](f: A => B): Case[A, B] =
      new shapeless.PolyDefns.Case1[this.type, A] {
        type Result = B
        val value = (l: A *: EmptyTuple) => f(l.head)
      }
  }
  trait Poly2 extends shapeless.Poly {
    final type Case[A, B, C] = shapeless.PolyDefns.Case2.Aux[this.type, A, B, C]
    @inline final def at[A, B, C](f: (A, B) => C): Case[A, B, C] =
      new shapeless.PolyDefns.Case2[this.type, A, B] {
        type Result = C
        val value = (l: A *: B *: EmptyTuple) => f(l.head, l.tail.head)
      }
  }
}
