package typify
package record

import scala.language.implicitConversions
import typify.tuple.Poly

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
    /**
     * Returns the value associated with the singleton typed key k. Only available if this record has a field with
     * with keyType equal to the singleton type K.
     */
    final def get[K <: Singleton](k: K)(using s: Selector[T, K]): s.Out = s(t)

    /**
     * Returns the value associated with the singleton typed key k. Only available if this record has a field with
     * with keyType equal to the singleton type K.
     */
    final def apply[K <: Singleton](k: K)(using s: Selector[T, K]): s.Out = s(t)

    /**
     * Returns the value associated with the singleton typed key k. Only available if this record has a field with
     * with keyType equal to the singleton type K.
     */
    final def fieldAt[K <: Singleton](k: K)(using s: Selector[T, K]): K ->> s.Out = label[K](s(t))

    /**
     * Updates or adds to this record a field with key k. The new field has a value of type V. Only available if this
     * record has a field with keyType equal to the singleton type K.
     */
    final def updated[K <: Singleton, V](k: K, v: V)(using u: Updater[T, K ->> V]): u.Out = u(t, label[K](v))

    /**
     * Replaces the value of field k with a value of the same type. Only available if this record has a field with
     * keyType equal to the singleton type K and valueType equal to V.
     */
    final def replace[K <: Singleton, V](k: K, v: V)(using s: Selector.Aux[T, K, V], u: Updater[T, K ->> V]): u.Out = u(t, label[K](v))

    /**
     * Updates a field having a value with type A by given function.
     */
    final def updateWith[K, A, B](s: SelectorFromKey.Aux[T, K, A])(f: A => B)(using m: Modifier[T, K, A, B]): m.Out = m(t, f)

    /**
     * Remove the field associated with the singleton typed key k, returning both the corresponding value and the updated
     * record. Only available if this record has a field with keyType equal to the singleton type K.
     */
    final def remove[K <: Singleton](k: K)(using r: Remover[T, K]): r.Out = r(t)

    /**
     * Updates or adds to this record a field of type F.
     */
    final def +[F](f: F)(using u: Updater[T, F]): u.Out = u(t, f)

    /**
     * Remove the field associated with the singleton typed key k, returning the updated record. Only available if this
     * record has a field with keyType equal to the singleton type K.
     */
    final def -[K <: Singleton, R, V, O](k: K)(using r: Remover.Aux[T, K, R], ev: R <:< (V, O)): O = r(t)._2

    /**
     * Returns the union of this record and another record.
     */
    final def merge[M](m: M)(using mg: Merger[T, M]): mg.Out = mg(t, m)

    /**
      * Extracts super-record from sub-record according to depth subtype relation
      */
    final def extract[E](using e: Extractor[T, E]): E = e(t)

    /**
      * Returns the union of this record and another record using the provided `f` to combine the values of fields which are present in both.
      *
      * The duplicated fields will be merged with `f`.
      */
    final def mergeWith[M](m: M)(f: Poly)(using mw: MergeWith[T, M, f.type]): mw.Out = mw(t, m)

    /**
     * Rename the field associated with the singleton typed key oldKey. Only available if this
     * record has a field with keyType equal to the singleton type K1.
     */
    final def renameField[K1 <: Singleton, K2 <: Singleton](oldKey: K1, newKey: K2)(using r: Renamer[T, K1, K2]): r.Out = r(t)

    /**
     * Returns the keys of this record as an `Tuple` of singleton typed values.
     */
    final def keys(implicit k: Keys[T]): k.Out = k()

    /**
     * Returns a `Tuple` of the values of this record.
     */
    final def values(implicit v: Values[T]): v.Out = v(t)

    /**
     * Returns a `Tuple` made of the key-value pairs of this record.
     */
    final def fields(implicit f: Fields[T]): f.Out = f(t)

    /**
     * Returns a `Map` whose keys and values are typed as the Lub of the keys
     * and values of this record.
     */
    final def toMap[K, V](implicit tm: ToMap.Aux[T, K, V]): Map[K, V] = tm(t)

    /**
     * Maps a higher rank function across the values of this record.
     */
    final def mapValues(f: Poly)(implicit m: MapValues[f.type, T]): m.Out = m(t)

    /**
      * Align the keys on the order of Tuple of keys K
      */
    final def alignByKeys[K <: Tuple](implicit a: AlignByKeys[T, K]): a.Out = a(t)
  }
}
