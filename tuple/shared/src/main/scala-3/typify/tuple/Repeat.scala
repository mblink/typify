package typify.tuple

import compiletime.ops.int.{-, >=}

type RepeatT[L <: Tuple, N <: Int] = N match {
  case 0 => EmptyTuple
  case _ => Tuple.Concat[RepeatT[L, N - 1], L]
}

/**
 * Typeclass supporting repeating a `Tuple` of type `L` `N` times.
 */
trait Repeat[L, N] extends DepFn1[L] with Serializable

object Repeat {
  type Aux[L, N, O] = Repeat[L, N] { type Out = O }

  inline def apply[L, N](using r: Repeat[L, N]): Repeat.Aux[L, N, r.Out] = r

  given tupleRepeat[L <: Tuple, N <: Int](
    using ev: (N >= 1) =:= true,
    n: ValueOf[N]
  ): Repeat.Aux[L, N, RepeatT[L, N]] =
    new Repeat[L, N] {
      type Out = RepeatT[L, N]
      def apply(l: L): Out = {
        var i: Int = n.value
        val b = collection.mutable.Buffer.empty[Object]
        lazy val a = l.toArray
        while (i > 0) {
          b ++= a
          i -= 1
        }
        Tuple.fromArray(b.to(Array)).asInstanceOf[Out]
      }
    }
}
