import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.validated._
import formless.hlist._
import scala.annotation.tailrec
import scala.language.implicitConversions

package object typify {
  type PV[P, L, A] = Cursor[P] => ValidatedNel[L, A]
  type KPV[P, L, A] = String => PV[P, L, A]

  type E2L[L, P] = ParseError[P] => L

  private[typify] def ap[A, B](a: A)(f: A => B): B = f(a)

  @inline implicit def toStringOps(s: String): StringOps = new StringOps(s)

  def parseList[A, L, O](cursor: Cursor[A])(
    fail: Cursor.Failed[A] => ValidatedNel[L, List[O]],
    proc: Cursor[A] => ValidatedNel[L, O]
  ): ValidatedNel[L, List[O]] = {
    lazy val empty = List[O]().validNel[L]

    cursor.downArray match {
      case f @ Cursor.Failed(_, _) => f.history.failedOp match {
        case Some(CursorOp.DownArray(true)) => empty
        case _ => fail(f)
      }
      case a =>
        @tailrec def go(c: Cursor[A], res: ValidatedNel[L, List[O]]): ValidatedNel[L, List[O]] =
          c match {
            case Cursor.Failed(_, _) => res
            case _ => go(c.right, (res, proc(c)).mapN(_ :+ _))
          }

        go(a, List[O]().validNel[L])
    }
  }

  def pvHNil[P, L]: PV[P, L, HNil] = (_: Cursor[P]) => HNil.validNel[L]
}
