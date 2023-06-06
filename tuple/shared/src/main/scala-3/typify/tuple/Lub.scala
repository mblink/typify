package typify.tuple

trait Lub[-A, -B, Out] {
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
