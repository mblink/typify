package typify.tuple

import compiletime.ops.int.>=

type GrouperT[L <: Tuple, N <: Int, Step <: Int] <: Tuple =
  (Tuple.Size[L] >= N) match {
    case true => Tuple.Take[L, N] *: GrouperT[Tuple.Drop[L, Step], N, Step]
    case false => EmptyTuple
  }

trait Grouper[L, N, Step] extends DepFn1[L]

object Grouper {
  type Aux[L, N, Step, O] = Grouper[L, N, Step] { type Out = O }

  inline def apply[L, N, Step](using g: Grouper[L, N, Step]): Grouper.Aux[L, N, Step, g.Out] = g

  given grouperTuple[L <: Tuple, N <: Int, Step <: Int](
    using nv: ValueOf[N],
    stepv: ValueOf[Step],
  ): Grouper.Aux[L, N, Step, GrouperT[L, N, Step]] =
    new Grouper[L, N, Step] {
      type Out = GrouperT[L, N, Step]

      private lazy val n = nv.value
      private lazy val step = stepv.value

      @annotation.tailrec
      private def go(a: Array[Object], acc: Array[Tuple]): Array[Tuple] =
        if (a.sizeIs >= n)
          go(a.drop(step), acc :+ Tuple.fromArray(a.take(n)))
        else
          acc

      def apply(l: L): Out = Tuple.fromArray(go(l.toArray, Array())).asInstanceOf[Out]
    }
}
