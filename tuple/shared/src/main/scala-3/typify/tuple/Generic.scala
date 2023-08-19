package typify.tuple

import scala.deriving.Mirror

trait Generic[A] {
  type Repr
  def to(a: A): Repr
  def from(r: Repr): A
}

object Generic {
  type Aux[A, R] = Generic[A] { type Repr = R }

  inline def apply[A](using g: Generic[A]): Generic.Aux[A, g.Repr] = g

  inline given productInst[A <: Product](using m: Mirror.ProductOf[A]): Generic.Aux[A, m.MirroredElemTypes] =
    new Generic[A] {
      type Repr = m.MirroredElemTypes
      def to(a: A): Repr = Tuple.fromProductTyped(a)
      def from(r: Repr): A = m.fromTuple(r)
    }
}
