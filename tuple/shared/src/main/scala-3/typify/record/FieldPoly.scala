package typify.record

import typify.tuple.Poly1

/**
 * Polymorphic function that allows modifications on record fields while preserving the original key types.
 */
trait FieldPoly extends Poly1 {
  final def atField[V]: [K <: Singleton] => K => [O] => (V => O) => Case.Aux[K ->> V, K ->> O] =
    [K <: Singleton] => (k: K) => [O] => (f: V => O) => at[K ->> V](kv => label[K](f(kv)))
}
