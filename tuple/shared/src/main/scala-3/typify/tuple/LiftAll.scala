package typify.tuple

import scala.compiletime.summonAll

/**
 * Typeclass witnessing that all the elements of a `Tuple` have instances of the given typeclass `F`.
 */
trait LiftAll[F[_], T] extends Serializable {
  type Out
  def instances: Out
}

object LiftAll {
  type Aux[F[_], T, O] = LiftAll[F, T] { type Out = O }

  final class Curried[F[_]](private val dummy: Boolean = false) extends AnyVal {
    final def apply[In](in: In)(using l: LiftAll[F, In]): LiftAll.Aux[F, In, l.Out] = l
  }

  def apply[F[_]]: Curried[F] = new Curried[F]
  def apply[F[_], In](using l: LiftAll[F, In]): LiftAll.Aux[F, In, l.Out] = l

  inline given tupleLiftAll[F[_], T <: Tuple]: LiftAll.Aux[F, T, Tuple.Map[T, F]] =
    new LiftAll[F, T] {
      type Out = Tuple.Map[T, F]
      val instances = summonAll[Tuple.Map[T, F]]
    }
}
