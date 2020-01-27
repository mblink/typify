package typify

import scala.collection.immutable.ListMap

trait Generic[A] {
  def toValues(value: A): Option[Vector[A]]
  def fromValues(values: Vector[A]): A

  def toFields(value: A): Option[ListMap[String, A]]
  def fromFields(fields: ListMap[String, A]): A
}

object Generic extends GenericAnyInstance
