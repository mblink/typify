package typify

import cats.data.NonEmptyVector
import cats.Eq
import cats.syntax.eq._

final case class CursorHistory(
  val successfulOps: Vector[CursorOp],
  val failedOps: Option[NonEmptyVector[CursorOp]]
) {
  def success: Boolean = failedOps.isEmpty

  def failedOp: Option[CursorOp] = failedOps.map(_.last)

  def toVector: Vector[CursorOp] = failedOps.fold(Vector[CursorOp]())(_.toVector) ++ successfulOps

  def :+(op: Either[CursorOp, CursorOp]): CursorHistory =
    op.fold(
      f => copy(failedOps = Some(failedOps.fold(NonEmptyVector.of(f))(_ :+ f))),
      s => copy(successfulOps = successfulOps :+ s))
}

object CursorHistory {
  lazy val empty: CursorHistory = CursorHistory(Vector(), None)

  implicit val eqCursorHistory: Eq[CursorHistory] =
    Eq.instance((a, b) => a.successfulOps === b.successfulOps && a.failedOps === b.failedOps)
}
