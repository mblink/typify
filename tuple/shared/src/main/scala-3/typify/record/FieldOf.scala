package typify.record

/**
 * Field with values of type `V`.
 *
 * Record keys of this form should be objects which extend this trait. Keys may also be arbitrary singleton typed
 * values, however keys of this form enforce the type of their values.
 */
trait FieldOf[V] {
  final type F = this.type ->> V
  inline final def ->>(v: V): this.type ->> V = label[this.type](v)
}
