package typify.record

import scala.language.implicitConversions
import typify.tuple.DepFn1

trait Selector[T, Key] extends DepFn1[T]

object Selector {
  type Aux[T, K, O] = Selector[T, K] { type Out = O }

  inline def apply[A, T](using s: Selector[A, T]): Aux[A, T, s.Out] = s

  inline given selectorInst[T <: Tuple, K](
    using idx: ValueOf[Tuple.Elem[FindField[T, K], 1]],
  ): Selector.Aux[T, K, Tuple.Head[FindField[T, K]]] =
    new Selector[T, K] {
      type Out = Tuple.Head[FindField[T, K]]
      def apply(t: T): Out = t.productElement(idx.value).asInstanceOf[Out]
    }
}

sealed trait SelectorFromKey[T <: Tuple, K] extends Selector[T, K]

object SelectorFromKey {
  type Aux[T <: Tuple, K, O] = SelectorFromKey[T, K] { type Out = O }

  implicit def selectorFromKeyInst[T <: Tuple, K <: Singleton](k: K)(using s: Selector[T, K]): SelectorFromKey.Aux[T, K, s.Out] =
    new SelectorFromKey[T, K] {
      type Out = s.Out
      def apply(t: T): Out = s(t)
    }
}
