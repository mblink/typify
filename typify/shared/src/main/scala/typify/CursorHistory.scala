package typify

import cats.data.NonEmptyVector
import cats.Eq
import cats.instances.option._
import cats.instances.vector._
import cats.syntax.eq._

class CursorHistory[A](
  val successfulOps: Vector[CursorOp],
  val failedOps: Option[NonEmptyVector[CursorOp]]
) {
  override final def toString: String = s"CursorHistory($successfulOps, $failedOps)"

  def success: Boolean = failedOps.isEmpty

  def failedOp: Option[CursorOp] = failedOps.map(_.last)

  def toVector: Vector[CursorOp] = failedOps.fold(Vector[CursorOp]())(_.toVector) ++ successfulOps

  def :+(op: Either[CursorOp, CursorOp]): CursorHistory[A] =
    op.fold(
      f => new CursorHistory[A](successfulOps, Some(failedOps.fold(NonEmptyVector.of(f))(_ :+ f))),
      s => new CursorHistory[A](successfulOps :+ s, failedOps))
}

object CursorHistory {
  def empty[A]: CursorHistory[A] = new CursorHistory(Vector(), None)

  implicit def eqCursorHistory[A]: Eq[CursorHistory[A]] =
    Eq.instance((a, b) => a.successfulOps === b.successfulOps && a.failedOps === b.failedOps)
}
