package typify
package labelled

private[typify] trait SelectorPackageAux {
  final type Selector[A <: shapeless.HList, Key] = shapeless.ops.record.Selector[A, Key]
  final val Selector: shapeless.ops.record.Selector.type = shapeless.ops.record.Selector

  @inline final def hlistToRecordOps[T <: shapeless.HList](t: T): shapeless.syntax.RecordOps[T] =
    new shapeless.syntax.RecordOps[T](t)
}
