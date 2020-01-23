package typify

import cats.Eq
import java.io.Serializable

sealed abstract class CursorOp extends Product with Serializable

final object CursorOp {
  final case object MoveLeft extends CursorOp
  final case object MoveRight extends CursorOp
  final case object MoveFirst extends CursorOp
  final case object MoveUp extends CursorOp
  final case class LeftN(n: Int) extends CursorOp
  final case class RightN(n: Int) extends CursorOp
  final case class Field(k: String) extends CursorOp
  final case class DownField(k: String) extends CursorOp
  final case class DownArray(empty: Boolean) extends CursorOp
  final case class DownN(n: Int, outOfRange: Boolean) extends CursorOp
  final case object DeleteGoParent extends CursorOp

  implicit final val eqCursorOp: Eq[CursorOp] = Eq.fromUniversalEquals

  val eqCursorOpList: Eq[List[CursorOp]] = cats.instances.list.catsKernelStdEqForList[CursorOp]

  def isDownField(op: CursorOp): Boolean =
    op match {
      case DownField(_) => true
      case _ => false
    }

  def isEmptyArray(op: CursorOp): Boolean =
    op match {
      case DownArray(e) => e
      case DownN(_, e) => e
      case _ => false
    }
}
