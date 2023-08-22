package typify.tuple

trait DepFn0 {
  type Out
  def apply(): Out
}

trait DepFn1[T] {
  type Out
  def apply(t: T): Out
}

trait DepFn2[T, U] {
  type Out
  def apply(t: T, u: U): Out
}

abstract class Case[F, L <: Tuple] extends Serializable {
  type Result
  val value: L => Result

  final def apply(t: L) = value(t)
  final def apply()(implicit ev: EmptyTuple =:= L) = value(EmptyTuple)
  final def apply[T](t: T)(implicit ev: (T *: EmptyTuple) =:= L) = value(t *: EmptyTuple)
  final def apply[T, U](t: T, u: U)(implicit ev: (T *: U *: EmptyTuple) =:= L) = value(t *: u *: EmptyTuple)
}

object Case {
  type Aux[F, L <: Tuple, R] = Case[F, L] { type Result = R }
  type Hom[F, T] = Case.Aux[F, T *: EmptyTuple, T]

  def apply[F, L <: Tuple, R](v: L => R): Case.Aux[F, L, R] =
    new Case[F, L] {
      type Result = R
      val value = v
    }
}

type Case0[F] = Case[F, EmptyTuple]
object Case0 {
  type Aux[F, R] = Case0[F] { type Result = R }

  def apply[F, R](r: => R): Case0.Aux[F, R] = Case(_ => r)
}

type Case1[F, A] = Case[F, A *: EmptyTuple]
object Case1 {
  type Aux[F, A, R] = Case1[F, A] { type Result = R }

  def apply[F, A, R](f: A => R): Case1.Aux[F, A, R] = Case { case (a *: EmptyTuple) => f(a) }
}

type Case2[F, A, B] = Case[F, A *: B *: EmptyTuple]
object Case2 {
  type Aux[F, A, B, R] = Case2[F, A, B] { type Result = R }

  def apply[F, A, B, R](f: (A, B) => R): Case2.Aux[F, A, B, R] = Case { case (a *: b *: EmptyTuple) => f(a, b) }
}

sealed trait Poly { self =>
  final def apply(using c: Case0[self.type]): c.Result = c()
  final def apply[A](a: A)(using c: Case1[self.type, A]): c.Result = c(a)
  final def apply[A, B](a: A, b: B)(using c: Case2[self.type, A, B]): c.Result = c(a, b)
}

trait Poly0 extends Poly { self =>
  final type Case = Case0[self.type]
  object Case {
    final type Aux[R] = Case0.Aux[self.type, R]
  }

  final inline def at[R](r: => R): Case.Aux[R] = Case0[self.type, R](r)
}

trait Poly1 extends Poly { self =>
  final type Case[A] = Case1[this.type, A]
  object Case {
    final type Aux[A, R] = Case1.Aux[self.type, A, R]
  }

  final inline def at[A] = [R] => (f: A => R) => Case1[self.type, A, R](f)
}

trait Poly2 extends Poly { self =>
  final type Case[A, B] = Case2[this.type, A, B]
  object Case {
    final type Aux[A, B, R] = Case2.Aux[self.type, A, B, R]
  }

  final inline def at[A, B] = [R] => (f: (A, B) => R) => Case2[self.type, A, B, R](f)
}

/**
 * Base class for lifting a `Function1` to a `Poly1`
 */
class ->[T, R](f: T => R) extends Poly1 {
  final given subT[U <: T]: Case.Aux[U, R] = at(f)
}

trait LiftFunction1LP extends Poly1 {
  final given default[T]: Case.Aux[T, EmptyTuple] = at(_ => EmptyTuple)
}

/**
 * Base class for lifting a `Function1` to a `Poly1` over the universal domain, yielding a `Tuple` with the result as
 * its only element if the argument is in the original functions domain, `EmptyTuple` otherwise.
 */
class >->[T, R](f: T => R) extends LiftFunction1LP {
  final given subT[U <: T]: Case.Aux[U, R *: EmptyTuple] = at(f(_) *: EmptyTuple)
}

// trait LiftULP extends Poly {
//   implicit def default[L <: HList] = new ProductCase[L] {
//     type Result = HNil
//     val value = (l: L) => HNil
//   }
// }

// /**
//  * Base class for lifting a `Poly` to a `Poly` over the universal domain, yielding a `Tuple` with the result as it's
//  * only element if the argument is in the original functions domain, `EmptyTuple` otherwise.
//  */
// class LiftU[P <: Poly](p: P)  extends LiftULP {
//   implicit def defined[L <: HList](implicit caseT: Case[P, L]) = new ProductCase[L] {
//     type Result = caseT.Result *: HNil
//     val value = (l: L) => caseT(l) *: HNil
//   }
// }
