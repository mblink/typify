import scalaz.{IndexedStateT, Monad, StateT, ValidationNel}
import scalaz.Validation.FlatMap._
import scalaz.syntax.validation._

package object typify {
  type E2L[L, P] = (Parsed[P], ParseError) => L
  type PV[P, L, A] = Parsed[P] => Validated[L, A]
  type KPV[P, L, A] = String => PV[P, L, A]

  type Validated[L, A] = StateT[ValidationNel[L, ?], Vector[Op], A]

  implicit def vmonad[L]: Monad[ValidationNel[L, ?]] = new Monad[ValidationNel[L, ?]] {
    def point[A0](a: => A0): ValidationNel[L, A0] = a.successNel[L]

    def bind[A0, B](fa: ValidationNel[L, A0])(f: A0 => ValidationNel[L, B]): ValidationNel[L, B] =
      fa.flatMap(f)
  }

  type ParsedValidated[A] = Validated[ParseError, A]

  def Validated[L, A](f: Vector[Op] => ValidationNel[L, (Vector[Op], A)]): Validated[L, A] =
    IndexedStateT.createState(_ => ops => f(ops))

  def ParsedValidated[A](f: Vector[Op] => ValidationNel[ParseError, (Vector[Op], A)]): ParsedValidated[A] =
    Validated(f)
}
