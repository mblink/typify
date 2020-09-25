package typify

import shapeless.{HList, Poly1}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Mapper
import shapeless.tag.@@

trait StringLabelledGeneric[A] extends shapeless.LabelledGeneric[A]

object StringLabelledGeneric {
  type Aux[A, R] = StringLabelledGeneric[A] { type Repr = R }

  def apply[A](implicit l: StringLabelledGeneric[A]): Aux[A, l.Repr] = l
  def toRecord[A](a: A)(implicit l: StringLabelledGeneric[A]): l.Repr = l.to(a)

  object stringify extends Poly1 {
    implicit def sym[K, A]: Case.Aux[FieldType[Symbol @@ K, A], FieldType[K, A]] =
      at(_.asInstanceOf[FieldType[K, A]])
  }

  object symbolify extends Poly1 {
    implicit def str[K, A]: Case.Aux[FieldType[K, A], FieldType[Symbol @@ K, A]] =
      at(_.asInstanceOf[FieldType[Symbol @@ K, A]])
  }

  implicit def inst[A, SymRepr <: HList, StrRepr <: HList](
    implicit lg: shapeless.LabelledGeneric.Aux[A, SymRepr],
    toStr: Mapper.Aux[stringify.type, SymRepr, StrRepr],
    fromStr: Mapper.Aux[symbolify.type, StrRepr, SymRepr]
  ): Aux[A, StrRepr] = new StringLabelledGeneric[A] {
    type Repr = StrRepr
    def to(a: A): StrRepr = toStr(lg.to(a))
    def from(r: StrRepr): A = lg.from(fromStr(r))
  }
}
