package typify
package labelled

trait Updater[L <: Tuple, F] {
  type Out
  def apply(l: L, f: F): Out
}

sealed trait UpdaterLP0 {
  final type Aux[L <: Tuple, F, Out0] = Updater[L, F] { type Out = Out0 }

  final type Appended[T <: Tuple, F] <: Tuple = T match {
    case EmptyTuple => F *: EmptyTuple
    case h *: t => h *: Appended[t, F]
  }

  final given appendUpdater[T <: Tuple, F]: Aux[T, F, Appended[T, F]] =
    new Updater[T, F] {
      type Out = Appended[T, F]
      def apply(t: T, f: F): Appended[T, F] = Tuple.fromArray[Any](t.toArray :+ (f: Any)).asInstanceOf[Appended[T, F]]
    }
}

sealed trait UpdaterLP1 extends UpdaterLP0 {
  final given tailUpdater[H, K <: Singleton, V, T <: Tuple](using u: Updater[T, K ->> V] { type Out <: Tuple }): Aux[H *: T, K ->> V, H *: u.Out] =
    new Updater[H *: T, K ->> V] {
      type Out = H *: u.Out
      def apply(t: H *: T, kv: K ->> V): H *: u.Out = t.head *: u(t.tail, kv)
    }
}

object Updater extends UpdaterLP1 {
  inline def apply[L <: Tuple, F](implicit u: Updater[L, F]): Aux[L, F, u.Out] = u
  inline def apply[L <: Tuple, F](l: L, f: F)(implicit u: Updater[L, F]): u.Out = u(l, f)

  /* TODO - revisit specializing Updater instances by generating this code:

  println(0.to(100).map { i =>
    val tps = 0.to(i - 1).map(j => s"A$j")
    val tupleTpe = (v: String) => s"${tps.mkString(" *: ")}${if (tps.isEmpty) "" else " *: "}(K ->> $v) *: EmptyTuple"
s"""
  given elem${i}Updater[${tps.mkString(", ")}${if (tps.isEmpty) "" else ", "}K <: Singleton, VI, VO]: Aux[${tupleTpe("VI")}, K ->> VO, ${tupleTpe("VO")}] =
    new Updater[${tupleTpe("VI")}, K ->> VO] {
      type Out = ${tupleTpe("VO")}
      def apply(t: ${tupleTpe("VI")}, f: K ->> VO): Out = Tuple.fromArray[Any](t.toArray.updated($i, f)).asInstanceOf[Out]
    }"""
  }.mkString("\n"))
  */

  given headUpdater[K, VI, VO, T <: Tuple]: Aux[(K ->> VI) *: T, K ->> VO, (K ->> VO) *: T] =
    new Updater[(K ->> VI) *: T, K ->> VO] {
      type Out = (K ->> VO) *: T
      def apply(t: (K ->> VI) *: T, f: K ->> VO): Out = f *: t.tail
    }
}
