package typify.record

final class TypifyMapOps[K, V](private val m: Map[K, V]) extends AnyVal {
  final def toRecord[R](using f: FromMap[R]): Option[R] = f(m)
}
