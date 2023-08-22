package typify.tuple

/**
 * Type class witnessing the least upper bound of a pair of types and providing conversions from each to their common supertype.
 */
trait Lub[-A, -B, Out] extends Serializable {
  def left(a: A): Out
  def right(b: B): Out
}

object Lub {
  given lub[T]: Lub[T, T, T] =
    new Lub[T, T, T] {
      def left(a: T): T = a
      def right(b: T): T = b
    }
}
