package typify
package labelled

trait Updater[L <: Tuple, F] {
  type Out
  def apply(l: L, f: F): Out
}

sealed trait UpdaterLP {
  final type Aux[L <: Tuple, F, Out0] = Updater[L, F] { type Out = Out0 }

  final type Appended[T <: Tuple, F] <: Tuple = T match {
    case EmptyTuple => F *: EmptyTuple
    case h *: t => h *: Appended[t, F]
  }

  given updaterTupleAppend[T <: Tuple, F]: Aux[T, F, Appended[T, F]] =
    new Updater[T, F] {
      type Out = Appended[T, F]
      def apply(t: T, f: F): Appended[T, F] = Tuple.fromArray(t.toArray :+ (f: Any)).asInstanceOf[Appended[T, F]]
    }
}

object Updater extends UpdaterLP {
  inline def apply[L <: Tuple, F](implicit u: Updater[L, F]): Aux[L, F, u.Out] = u
  inline def apply[L <: Tuple, F](l: L, f: F)(implicit u: Updater[L, F]): u.Out = u(l, f)

  given updaterTupleHead[K, VI, VO, T <: Tuple]: Aux[(K ->> VI) *: T, K ->> VO, (K ->> VO) *: T] =
    new Updater[(K ->> VI) *: T, K ->> VO] {
      type Out = (K ->> VO) *: T
      def apply(t: (K ->> VI) *: T, kv: K ->> VO): (K ->> VO) *: T = kv *: (t.tail: T)
    }

  given updaterTupleTail[H, K, V, T <: Tuple](using u: Updater[T, K ->> V] { type Out <: Tuple }): Aux[H *: T, K ->> V, H *: u.Out] =
    new Updater[H *: T, K ->> V] {
      type Out = H *: u.Out
      def apply(t: H *: T, kv: K ->> V): H *: u.Out = t.head *: u(t.tail, kv)
    }
}
