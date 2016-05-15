package typify

import scala.language.higherKinds
import shapeless.{HList, LabelledGeneric}
import shapeless.ops.hlist.Align
import shapeless.ops.record.RemoveAll

trait Converter[G <: HList, A] {
  def convert(g: G): A
}

trait NoExtraConverter {
  implicit def convert[A, F <: HList, G <: HList](implicit
      gen: LabelledGeneric.Aux[A, F],
      align: Align[G, F]): Converter[G, A] =
    new Converter[G, A] {
      def convert(g: G) = gen.from(align(g))
    }
}

object convert extends NoExtraConverter {

  implicit class ToHList[A, G <: HList, R <: HList](a: A)(implicit
      gen: LabelledGeneric.Aux[A, R]) {
        def toHList: R = gen.to(a)
  }

  implicit def convertToSubset[A, F <: HList, G <: HList, H <: HList](implicit
      gen: LabelledGeneric.Aux[A, F],
      rma: RemoveAll.Aux[G, F, (F, H)]): Converter[G, A] =
    new Converter[G, A] {
      def convert(g: G) = gen.from(rma(g)._1)
    }

  implicit class ToConverter[G <: HList](g: G) {
    def convertTo[B](implicit c: Converter[G, B]): B =
      c.convert(g)
  }
}
