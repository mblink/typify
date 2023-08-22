package typify
package record

import scala.language.implicitConversions

trait RecordPackageCompat {
  final opaque type ->>[K, +V] = tagged.TranslucentTagged[V, K]
  object ->> {
    implicit def convertToV[K, V](kv: K ->> V): V = kv
  }

  @inline final def field[K]: [V] => V => (K ->> V) = [v] => (v: v) => tagged.translucentTag[K](v)
  @inline final def label[K]: [V] => V => (K ->> V) = [v] => (v: v) => tagged.translucentTag[K](v)

  extension[K <: Singleton](k: K) {
    inline final def ->>[V](v: V): K ->> V = tagged.translucentTag[K](v)
  }

  extension[K, V](kv: K ->> V) {
    inline final def label(using k: ValueOf[K]): K = k.value
  }

  final implicit def toTypifyRecordOps[T <: Tuple](t: T): TypifyRecordOps[T] = new TypifyRecordOps(t)

  final implicit def toTypifyMapOps[K, V](m: Map[K, V]): TypifyMapOps[K, V] = new TypifyMapOps(m)
}
