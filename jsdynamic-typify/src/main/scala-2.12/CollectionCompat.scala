package scala.scalajs.js.typify

import scala.collection.immutable.ListMap

trait CollectionCompat {
  implicit class ListMapObjectOps(lm: ListMap.type) {
    def from[K, V](x: TraversableOnce[(K, V)]): ListMap[K, V] = (ListMap.newBuilder ++= x).result()
  }
}
