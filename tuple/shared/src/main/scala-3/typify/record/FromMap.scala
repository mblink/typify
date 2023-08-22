package typify.record

import scala.compiletime.{erasedValue, summonInline}
import scala.reflect.TypeTest

trait FromMap[R] extends Serializable {
  def apply[K, V](m: Map[K, V]): Option[R]
}

object FromMap {
  inline def apply[R](using f: FromMap[R]): FromMap[R] = f

  private inline def tupleFromMapRec[T <: Tuple, K, V](m: Map[K, V]): Option[Tuple] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Some(EmptyTuple)
      case _: ((k, v) *: t) =>
        for {
          v <- m.get(summonInline[ValueOf[k]].value.asInstanceOf[K])
          typed <- summonInline[TypeTest[V, v]].unapply(v)
          rest <- tupleFromMapRec[t, K, V](m)
        } yield label[k](typed) *: rest
    }

  inline given tupleFromMap[T <: Tuple](using ev: IsRecord[T] =:= true): FromMap[T] =
    new FromMap[T] {
      def apply[K, V](m: Map[K, V]): Option[T] = tupleFromMapRec[FieldsT[T], K, V](m).asInstanceOf[Option[T]]
    }
}
