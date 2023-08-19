package typify.record

trait UnzipFields[L] extends Serializable {
  type Keys
  type Values
  def keys: Keys
  def values(l: L): Values
}

object UnzipFields {
  type Aux[L, K, V] = UnzipFields[L] {
    type Keys = K
    type Values = V
  }

  inline def apply[L](using u: UnzipFields[L]): UnzipFields.Aux[L, u.Keys, u.Values] = u

  given tupleUnzipFields[L <: Tuple](using k: Keys[L], v: Values[L]): UnzipFields.Aux[L, k.Out, v.Out] =
    new UnzipFields[L] {
      type Keys = k.Out
      type Values = v.Out
      lazy val keys = k()
      def values(l: L): Values = v(l)
    }
}
