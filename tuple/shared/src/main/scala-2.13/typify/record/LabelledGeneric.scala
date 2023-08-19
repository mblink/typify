package typify.record

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import shapeless.{Coproduct, HList}
import shapeless.ops.coproduct.{ZipWithKeys => CoproductZipWithKeys}
import shapeless.ops.hlist.{ZipWithKeys => HListZipWithKeys}

trait LabelledGeneric[A] extends shapeless.LabelledGeneric[A]

object LabelledGeneric {
  type Aux[A, R] = LabelledGeneric[A] { type Repr = R }

  def apply[A](implicit l: LabelledGeneric[A]): LabelledGeneric.Aux[A, l.Repr] = l
  def toRecord[A](a: A)(implicit l: LabelledGeneric[A]): l.Repr = l.to(a)

  implicit def hlistInst[A, K <: HList, V <: HList, R <: HList](
    implicit @annotation.unused l: StringLabelling.Aux[A, K],
    g: shapeless.Generic.Aux[A, V],
    @annotation.unused z: HListZipWithKeys.Aux[K, V, R],
  ): LabelledGeneric.Aux[A, R] =
    new LabelledGeneric[A] {
      type Repr = R
      def to(a: A): R = g.to(a).asInstanceOf[R]
      def from(r: R): A = g.from(r.asInstanceOf[V])
    }

  implicit def coproductInst[A, K <: HList, V <: Coproduct, R <: Coproduct](
    implicit @annotation.unused l: StringLabelling.Aux[A, K],
    g: shapeless.Generic.Aux[A, V],
    @annotation.unused z: CoproductZipWithKeys.Aux[K, V, R],
  ): LabelledGeneric.Aux[A, R] =
    new LabelledGeneric[A] {
      type Repr = R
      def to(a: A): R = g.to(a).asInstanceOf[R]
      def from(r: R): A = g.from(r.asInstanceOf[V])
    }
}

trait StringLabelling[A] { type Labels }

object StringLabelling {
  type Aux[A, L] = StringLabelling[A] { type Labels = L }

  implicit def inst[A]: StringLabelling[A] = macro Macros.instImpl[A]

  final class Macros(override val c: whitebox.Context) extends shapeless.LabelledMacros(c) {
    import c.universe._

    private def constNameTpe(n: Name): Type = c.internal.constantType(Constant(nameAsString(n)))

    final def instImpl[T: WeakTypeTag]: Tree = {
      val tTpe = weakTypeOf[T]
      val labelsTpe =
        if (isProduct(tTpe))
          fieldsOf(tTpe).foldRight(hnilTpe)((f, acc) => appliedType(hconsTpe, List(constNameTpe(f._1), acc)))
        else if (isCoproduct(tTpe))
          ctorsOf(tTpe).foldRight(hnilTpe)((c, acc) => appliedType(hconsTpe, List(constNameTpe(nameOf(c)), acc)))
        else
          c.abort(c.enclosingPosition, s"$tTpe is not case class like or the root of a sealed family of types")

      q"""(new _root_.typify.record.StringLabelling[$tTpe] { type Labels = $labelsTpe })
        .asInstanceOf[_root_.typify.record.StringLabelling.Aux[$tTpe, $labelsTpe]]"""
    }
  }
}
