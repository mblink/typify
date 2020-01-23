import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.validated._
import scala.annotation.tailrec

package object typify extends typify.StringOps {
  type PV[P, L, A] = Cursor[P] => ValidatedNel[L, A]
  type KPV[P, L, A] = String => PV[P, L, A]

  type E2L[L, P] = ParseError[P] => L

  private[typify] def ap[A, B](a: A)(f: A => B): B = f(a)

  def parseList[A, L, O](cursor: Cursor[A])(
    fail: Cursor.Failed[A] => ValidatedNel[L, List[O]],
    proc: Cursor[A] => ValidatedNel[L, O]
  ): ValidatedNel[L, List[O]] =
    cursor.downArray match {
      case f @ Cursor.Failed(_, _) => fail(f)
      case a =>
        @tailrec def go(c: Cursor[A], res: ValidatedNel[L, List[O]]): ValidatedNel[L, List[O]] =
          c match {
            case Cursor.Failed(_, _) => res
            case _ => go(c.right, (res, proc(c)).mapN(_ :+ _))
          }

        go(a, List[O]().validNel[L])
    }
}
