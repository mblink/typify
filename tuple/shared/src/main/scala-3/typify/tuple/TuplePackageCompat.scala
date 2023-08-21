package typify.tuple

import scala.language.implicitConversions

trait TuplePackageCompat {
  final type Tuple = scala.Tuple
  final type *:[H, T <: Tuple] = scala.*:[H, T]
  final val *: : scala.*:.type = scala.*:
  final type EmptyTuple = scala.EmptyTuple
  final val EmptyTuple: EmptyTuple = scala.EmptyTuple

  @inline final implicit def toTypifyTupleOps[L <: Tuple](l: L): TypifyTupleOps[L] = new TypifyTupleOps[L](l)
}
