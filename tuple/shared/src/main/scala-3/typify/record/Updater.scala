package typify.record

import typify.tuple.{AppendT, ElemIndexWithFallback, DepFn2, IfEq}

/**
 * Type class supporting record update and extension.
 */
trait Updater[T, F] extends DepFn2[T, F]

object Updater {
  type Aux[T, F, O] = Updater[T, F] { type Out = O }

  inline def apply[T, F](implicit u: Updater[T, F]): Updater.Aux[T, F, u.Out] = u
  inline def apply[T, F](t: T, f: F)(implicit u: Updater[T, F]): u.Out = u(t, f)

  inline given updaterInst[T <: Tuple, F](
    using idx: ValueOf[ElemIndexWithFallback[T, F]]
  ): Updater.Aux[T, F, IfEq[ElemIndexWithFallback[T, F], -1, AppendT[T, F], T]] =
    new Updater[T, F] {
      type Out = IfEq[ElemIndexWithFallback[T, F], -1, AppendT[T, F], T]
      def apply(t: T, f: F): Out =
        Tuple.fromArray(if (idx.value == -1) t.toArray :+ f.asInstanceOf[Object] else {
          val a = t.toArray
          a.update(idx.value, f.asInstanceOf[Object])
          a
        }).asInstanceOf[Out]
    }
}
