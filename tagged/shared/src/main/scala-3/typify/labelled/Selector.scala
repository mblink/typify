package typify
package labelled

private[typify] trait SelectorPackageAux

trait Selector[A, Key] {
  type Out
  def apply(a: A): Out
}

object Selector {
  type Aux[A, K, O] = Selector[A, K] { type Out = O }

  inline def apply[A, T](using s: Selector[A, T]): Aux[A, T, s.Out] = s

  given head[Key, Head, Tail <: Tuple]: Aux[(Key ->> Head) *: Tail, Key, Head] =
    new Selector[(Key ->> Head) *: Tail, Key] {
      type Out = Head
      def apply(t: (Key ->> Head) *: Tail): Head = t.head
    }

  given tail[Key, Head, Tail <: Tuple](using s: Selector[Tail, Key]): Aux[Head *: Tail, Key, s.Out] =
    new Selector[Head *: Tail, Key] {
      type Out = s.Out
      def apply(t: Head *: Tail): s.Out = s(t.tail)
    }
}
