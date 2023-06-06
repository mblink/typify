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

opaque type Case0[F, A] = () => A
object Case0 {
  inline def apply[F, A](f: () => A): Case0[F, A] = f
  extension [F, A](f: Case0[F, A]) inline def run(): A = f()
}
opaque type Case1[F, A, B] = A => B
object Case1 {
  inline def apply[F, A, B](f: A => B): Case1[F, A, B] = f
  extension [F, A, B](f: Case1[F, A, B]) inline def run(a: A): B = f(a)
}
opaque type Case2[F, A, B, C] = (A, B) => C
object Case2 {
  inline def apply[F, A, B, C](f: (A, B) => C): Case2[F, A, B, C] = f
  extension [F, A, B, C](f: Case2[F, A, B, C]) inline def run(a: A, b: B): C = f(a, b)
}

sealed trait Poly
trait Poly0 extends Poly {
  final type Case[A] = Case0[this.type, A]
  final inline def at[A](f: () => A): Case[A] = Case0(f)
}
trait Poly1 extends Poly {
  final type Case[A, B] = Case1[this.type, A, B]
  final inline def at[A, B](f: A => B): Case[A, B] = Case1(f)
}
trait Poly2 extends Poly {
  final type Case[A, B, C] = Case2[this.type, A, B, C]
  final inline def at[A, B, C](f: (A, B) => C): Case[A, B, C] = Case2(f)
}
