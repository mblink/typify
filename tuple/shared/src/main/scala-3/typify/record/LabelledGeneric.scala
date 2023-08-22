package typify.record

import scala.deriving.Mirror
import typify.tuple.ZipWithT

trait LabelledGeneric[A] extends Serializable {
  type Repr
  def from(r: Repr): A
  def to(a: A): Repr
}

object LabelledGeneric {
  type Aux[A, R] = LabelledGeneric[A] { type Repr = R }

  inline def apply[A](using l: LabelledGeneric[A]): LabelledGeneric.Aux[A, l.Repr] = l
  def toRecord[A](a: A)(using l: LabelledGeneric[A]): l.Repr = l.to(a)

  inline given productInst[A <: Product](
    using m: Mirror.ProductOf[A]
  ): LabelledGeneric.Aux[A, ZipWithT[m.MirroredElemLabels, m.MirroredElemTypes, ->>]] =
    new LabelledGeneric[A] {
      type Repr = Tuple & ZipWithT[m.MirroredElemLabels, m.MirroredElemTypes, ->>]
      def from(r: Repr): A = m.fromTuple(r.asInstanceOf[m.MirroredElemTypes])
      def to(a: A): Repr = Tuple.fromProductTyped(a).asInstanceOf[Repr]
    }

  inline given sumInst[A](
    using m: Mirror.SumOf[A]
  ): LabelledGeneric.Aux[A, Tuple.Union[ZipWithT[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]] =
    new LabelledGeneric[A] {
      type Repr = Tuple.Union[ZipWithT[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]
      def from(r: Repr): A = r.asInstanceOf[A]
      def to(a: A): Repr = a.asInstanceOf[Repr]
    }
}
