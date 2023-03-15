package typify
package labelled

private[typify] trait LabelledPackageAux

extension[K <: Singleton](k: K) {
  inline final def ->>[V](v: V): K ->> V = label[K](v)
}

extension[T <: Tuple](t: T) {
  inline def get[K <: Singleton](k: K)(using s: Selector[T, K]): s.Out = s(t)
  inline def apply[K <: Singleton](k: K)(using s: Selector[T, K]): s.Out = s(t)

  inline def updated[K <: Singleton, V](k: K, v: V)(using u: Updater[T, K ->> V]): u.Out =
    u(t, label[K](v))

  inline def updateWith[K <: Singleton, V](k: K)(
    using s: Selector[T, K]
  )(f: s.Out => V)(
    using u: Updater[T, K ->> V]
  ): u.Out =
    u(t, label[K](f(s(t))))

  inline def -[K <: Singleton, V, O](k: K)(using r: Remover.Aux[T, K, (V, O)]): O =
    r(t)._2

  inline def merge[M <: Tuple](m: M)(using mg: Merger[T, M]): mg.Out =
    mg(t, m)

  inline def keys(using k: Keys[T]): k.Out =
    k()

  inline def renameField[K1 <: Singleton, K2 <: Singleton](oldKey: K1, newKey: K2)(using r: Renamer[T, K1, K2]): r.Out =
    r(t)
}
