package typify

import scala.language.implicitConversions

final class HListTupleOps[H <: shapeless.HList](private val h: H) extends AnyVal {
  @inline final def *:[A](a: A): shapeless.::[A, H] = a :: h
}

final class LabelledOps[K, A](private val l: typify.Labelled[K, A]) {
  @inline final def value: A = l
}

private[typify] trait Compat {
  type Tuple = shapeless.HList

  type EmptyTuple = shapeless.HNil
  val EmptyTuple: shapeless.HNil.type = shapeless.HNil

  type *:[H, T <: Tuple] = shapeless.::[H, T]
  @inline implicit def toHListTupleOps[H <: shapeless.HList](h: H): HListTupleOps[H] = new HListTupleOps[H](h)

  type Labelled[K, A] = shapeless.labelled.FieldType[K, A]
  type ->>[K, A] = Labelled[K, A]
  @inline def Labelled[K]: shapeless.labelled.FieldBuilder[K] = shapeless.labelled.field[K]
  @inline implicit def toLabelledOps[K, A](l: Labelled[K, A]): LabelledOps[K, A] = new LabelledOps[K, A](l)
}
