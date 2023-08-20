package typify
package record

import scala.language.implicitConversions

trait RecordPackageCompat {
  final opaque type ->>[K, +V] = tagged.TranslucentTagged[V, K]
  object ->> {
    implicit def convertToV[K, V](kv: K ->> V): V = kv
  }

  @inline final def label[K]: [V] => V => (K ->> V) = [v] => (v: v) => tagged.translucentTag[K](v)

  extension[K <: Singleton](k: K) {
    inline final def ->>[V](v: V): K ->> V = tagged.translucentTag[K](v)
  }

  extension[K, V](kv: K ->> V) {
    inline final def label(using k: ValueOf[K]): K = k.value
  }

  extension[T <: Tuple](t: T) {
    def get[K <: Singleton](k: K)(using s: Selector[T, K]): s.Out = s(t)

    def apply[K <: Singleton](k: K)(using s: Selector[T, K]): s.Out = s(t)

    def fieldAt[K <: Singleton](k: K)(using s: Selector[T, K]): K ->> s.Out = label[K](s(t))

    def updated[K <: Singleton, V](k: K, v: V)(using u: Updater[T, K ->> V]): u.Out = u(t, label[K](v))

    def updateWith[K, A, B](s: SelectorFromKey.Aux[T, K, A])(f: A => B)(using m: Modifier[T, K, A, B]): m.Out = m(t, f)

    def -[K <: Singleton, R, V, O](k: K)(using r: Remover.Aux[T, K, R], ev: R <:< (V, O)): O = r(t)._2

    def merge[M <: Tuple](m: M)(using mg: Merger[T, M]): mg.Out = mg(t, m)

    def keys(using k: Keys[T]): k.Out = k()

    def renameField[K1 <: Singleton, K2 <: Singleton](oldKey: K1, newKey: K2)(using r: Renamer[T, K1, K2]): r.Out = r(t)
  }
}
