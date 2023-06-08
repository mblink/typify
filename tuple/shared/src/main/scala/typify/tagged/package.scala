package typify

package object tagged extends OpaquePackageAux with TranslucentPackageAux {
  type Tagged[+A, T] = OpaqueTagged[A, T]
  type @@[+A, T] = Tagged[A, T]

  @inline def tag[T]: OpaqueTagged.Of[T] = opaqueTag[T]
}
