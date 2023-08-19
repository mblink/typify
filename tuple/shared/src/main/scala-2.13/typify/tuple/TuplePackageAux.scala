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

  final type Comapped[T <: Tuple, F[_]] = shapeless.ops.hlist.Comapped[T, F]
  final val Comapped: shapeless.ops.hlist.Comapped.type = shapeless.ops.hlist.Comapped

  final type ConstMapper[C, T <: Tuple] = shapeless.ops.hlist.ConstMapper[C, T]
  final val ConstMapper: shapeless.ops.hlist.ConstMapper.type = shapeless.ops.hlist.ConstMapper

  final type Generic[A] = shapeless.Generic[A]
  final val Generic: shapeless.Generic.type = shapeless.Generic

  final type FillWith[F, L <: Tuple] = shapeless.ops.hlist.FillWith[F, L]
  final val FillWith: shapeless.ops.hlist.FillWith.type = shapeless.ops.hlist.FillWith

  final type FlatMapper[F, In <: Tuple] = shapeless.ops.hlist.FlatMapper[F, In]
  final val FlatMapper: shapeless.ops.hlist.FlatMapper.type = shapeless.ops.hlist.FlatMapper

  final type Init[T <: Tuple] = shapeless.ops.hlist.Init[T]
  final val Init: shapeless.ops.hlist.Init.type = shapeless.ops.hlist.Init

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

  final type MapFolder[T <: Tuple, R, F] = shapeless.ops.hlist.MapFolder[T, R, F]
  final val MapFolder: shapeless.ops.hlist.MapFolder.type = shapeless.ops.hlist.MapFolder

  final type Mapped[T <: Tuple, F[_]] = shapeless.ops.hlist.Mapped[T, F]
  final val Mapped: shapeless.ops.hlist.Mapped.type = shapeless.ops.hlist.Mapped

  final type Mapper[F, In <: Tuple] = shapeless.ops.hlist.Mapper[F, In]
  final val Mapper: shapeless.ops.hlist.Mapper.type = shapeless.ops.hlist.Mapper

  final type Prepend[L <: Tuple, R <: Tuple] = shapeless.ops.hlist.Prepend[L, R]
  final val Prepend: shapeless.ops.hlist.Prepend.type = shapeless.ops.hlist.Prepend

  final type Reify[T <: Tuple] = shapeless.ops.hlist.Reify[T]
  final val Reify: shapeless.ops.hlist.Reify.type = shapeless.ops.hlist.Reify

  final type RightFolder[L <: Tuple, In, F] = shapeless.ops.hlist.RightFolder[L, In, F]
  final val RightFolder: shapeless.ops.hlist.RightFolder.type = shapeless.ops.hlist.RightFolder

  final type RightReducer[L <: Tuple, F] = shapeless.ops.hlist.RightReducer[L, F]
  final val RightReducer: shapeless.ops.hlist.RightReducer.type = shapeless.ops.hlist.RightReducer

  final type SubtypeUnifier[T <: Tuple, B] = shapeless.ops.hlist.SubtypeUnifier[T, B]
  final val SubtypeUnifier: shapeless.ops.hlist.SubtypeUnifier.type = shapeless.ops.hlist.SubtypeUnifier

  final type ToArray[L <: Tuple, Lub] = ToTraversable.Aux[L, Array, Lub]
  final def ToArray[L <: Tuple, Lub](implicit t: ToArray[L, Lub]): ToArray[L, Lub] = t

  final type ToList[L <: Tuple, Lub] = ToTraversable.Aux[L, List, Lub]
  final def ToList[L <: Tuple, Lub](implicit t: ToList[L, Lub]): ToList[L, Lub] = t

  final type ToTraversable[L <: Tuple, M[_]] = shapeless.ops.hlist.ToTraversable[L, M]
  final val ToTraversable: shapeless.ops.hlist.ToTraversable.type = shapeless.ops.hlist.ToTraversable

  final type TupleSelector[T <: Tuple, A] = shapeless.ops.hlist.Selector[T, A]
  final val TupleSelector: shapeless.ops.hlist.Selector.type = shapeless.ops.hlist.Selector

  final type Unifier[T <: Tuple] = shapeless.ops.hlist.Unifier[T]
  final val Unifier: shapeless.ops.hlist.Unifier.type = shapeless.ops.hlist.Unifier

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
