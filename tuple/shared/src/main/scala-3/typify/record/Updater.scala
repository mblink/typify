package typify.record

import typify.tuple.{Append, DepFn2, IfEq, IndexOf}

trait Updater[T <: Tuple, F] extends DepFn2[T, F]

object Updater {
  type Aux[T <: Tuple, F, O] = Updater[T, F] { type Out = O }

  inline def apply[T <: Tuple, F](implicit u: Updater[T, F]): Aux[T, F, u.Out] = u
  inline def apply[T <: Tuple, F](t: T, f: F)(implicit u: Updater[T, F]): u.Out = u(t, f)

  inline given updaterInst[T <: Tuple, F](
    using idx: ValueOf[IndexOf[T, F]]
  ): Updater.Aux[T, F, IfEq[IndexOf[T, F], -1, Append[T, F], T]] =
    new Updater[T, F] {
      type Out = IfEq[IndexOf[T, F], -1, Append[T, F], T]
      def apply(t: T, f: F): Out =
        Tuple.fromArray(if (idx.value == -1) t.toArray :+ f.asInstanceOf[Object] else {
          val a = t.toArray
          a.update(idx.value, f.asInstanceOf[Object])
          a
        }).asInstanceOf[Out]
    }
}
