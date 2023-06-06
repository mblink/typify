package typify.labelled

import scala.deriving.Mirror

type ZipWith[T1 <: Tuple, T2 <: Tuple, F[_, _]] <: Tuple = (T1, T2) match {
  case (h1 *: t1, h2 *: t2) => F[h1, h2] *: ZipWith[t1, t2, F]
  case (EmptyTuple, ?) => EmptyTuple
  case (?, EmptyTuple) => EmptyTuple
}

trait LabelledGeneric[A] {
  type Repr
  def from(r: Repr): A
  def to(a: A): Repr
}

object LabelledGeneric {
  type Aux[A, R] = LabelledGeneric[A] { type Repr = R }

  inline def apply[A](using l: LabelledGeneric[A]): Aux[A, l.Repr] = l
  def toRecord[A](a: A)(using l: LabelledGeneric[A]): l.Repr = l.to(a)

  implicit inline def inst[A <: Product](using
  m: Mirror.ProductOf[A]): Aux[A, ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, ->>]] =
    new LabelledGeneric[A] {
      type Repr = Tuple & ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, ->>]
      def from(r: Repr): A = m.fromTuple(r.asInstanceOf[m.MirroredElemTypes]) // scalafix:ok DisableSyntax.asInstanceOf
      def to(a: A): Repr = Tuple.fromProductTyped(a).asInstanceOf[Repr] // scalafix:ok DisableSyntax.asInstanceOf
    }

  implicit inline def inst[A](using
  m: Mirror.SumOf[A]): Aux[A, Tuple.Union[ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]] =
    new LabelledGeneric[A] {
      type Repr = Tuple.Union[ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]
      def from(r: Repr): A = r.asInstanceOf[A] // scalafix:ok DisableSyntax.asInstanceOf
      def to(a: A): Repr = a.asInstanceOf[Repr] // scalafix:ok DisableSyntax.asInstanceOf
    }
}
