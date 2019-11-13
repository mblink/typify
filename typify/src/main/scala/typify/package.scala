import scalaz.{ReaderWriterStateT, ValidationNel}
import scalaz.syntax.validation._
import shapeless.syntax.std.tuple._

package object typify {
  type E2L[L, P] = (Parsed[P], ParseError) => L
  type PV[P, L, A] = Parsed.Aux[P, P] => Validated0[L, A]
  type KPV[P, L, A] = String => PV[P, L, A]

  type Validated0[L, A] = ReaderWriterStateT[ValidationNel[L, ?], Vector[Op], Vector[Op], Unit, A]
  type Validated[A] = Validated0[ParseError, A]

  def Validated[A](f: Vector[Op] => ValidationNel[ParseError, (Vector[Op], A)]): Validated[A] =
    ReaderWriterStateT((ops, _) => f(ops).map(_ :+ (())))

  def Validated0[L, A](a: A): Validated0[L, A] =
    ReaderWriterStateT((_: Vector[Op], _: Unit) => (Vector[Op](), a, ()).successNel[L])
}
