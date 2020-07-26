package typify

import shapeless.{::, HList, HNil}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{RightFolder, Prepend}
import typify.Optimize._

object Optimize {
  trait TWitness[T] { type Out = T }

  object syntax {

    def typeOf[A] = new TWitness[A] {}

    implicit class FieldTypeStub[K, A](val ft: FieldType[K, TWitness[A]]) {

      def witness = {
        new TWitness[FieldType[K, A]] {}
      }
    }

    implicit class TWitnessOps[T](tw: TWitness[T]) {

      def +[A](otw: TWitness[A])(implicit p: Prepend[T :: HNil, otw.Out :: HNil]) =
        new TWitness[p.Out] {}
    }

    implicit class TWitnessHlOps[T <: HList](tw: TWitness[T]) {

      def +[A](otw: TWitness[A])(implicit p: Prepend[T, otw.Out :: HNil]) =
        new TWitness[p.Out] {}

      def ++[A <: HList](otw: TWitness[A])(implicit p: Prepend[T, otw.Out]) =
        new TWitness[p.Out] {}

      def converter[A](implicit c: Converter[T, A]) = c
    }
  }
}

class Optimize[L, P](val tp: Typify[L, P]) {
  def folder[G <: HList, A, R <: HList](@deprecated("unused", "") in: G)(
    implicit rf: RightFolder.Aux[G, tp.PV[HNil], foldPV.type, A]
  ): RightFolder.Aux[G, tp.PV[HNil], foldPV.type, A] = rf

  def success[G <: HList, A, R <: HList](@deprecated("unused", "")  in: G)(
    implicit @deprecated("unused", "")  rf: RightFolder.Aux[G, tp.PV[HNil], foldPV.type, A],
    @deprecated("unused", "")  pvEv: A <:< tp.PV[R]
  ): TWitness[R] = new TWitness[R] {}
}
