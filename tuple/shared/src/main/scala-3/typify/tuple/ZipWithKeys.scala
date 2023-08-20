package typify.tuple

import typify.record.{->>, label}

/**
 * Type class supporting zipping a `Tuple` of values with a `Tuple` of keys to create a record.
 */
trait ZipWithKeys[K, V] extends DepFn1[V]

object ZipWithKeys {
  type Aux[K, V, O] = ZipWithKeys[K, V] { type Out = O }

  inline def apply[K, V](using z: ZipWithKeys[K, V]): ZipWithKeys.Aux[K, V, z.Out] = z

  object F extends Poly2 {
    given kv[K, V]: Case[K, V, K ->> V] = at((_, v) => label[K](v))
  }

  inline given tupleZipWithKeys[K <: Tuple, V <: Tuple](
    using z: ZipWith[K, V, F.type],
  ): ZipWithKeys.Aux[K, V, z.Out] =
    new ZipWithKeys[K, V] {
      type Out = z.Out
      def apply(v: V): Out = z(compiletime.constValueTuple[K], v)
    }
}
