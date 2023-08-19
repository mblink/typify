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

  final type Generic[A] = shapeless.Generic[A]
  final val Generic: shapeless.Generic.type = shapeless.Generic

  final type FillWith[F, L <: Tuple] = shapeless.ops.hlist.FillWith[F, L]
  final val FillWith: shapeless.ops.hlist.FillWith.type = shapeless.ops.hlist.FillWith

  final type IsNonEmptyTuple[T <: Tuple] = shapeless.ops.hlist.IsHCons[T]
  final val IsNonEmptyTuple: shapeless.ops.hlist.IsHCons.type = shapeless.ops.hlist.IsHCons

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

  final type ToList[T <: Tuple, Lub] = shapeless.ops.hlist.ToList[T, Lub]

  final type TupleSelector[T <: Tuple, A] = shapeless.ops.hlist.Selector[T, A]
  final val TupleSelector: shapeless.ops.hlist.Selector.type = shapeless.ops.hlist.Selector

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
