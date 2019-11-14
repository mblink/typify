package typify

import org.scalacheck.{Arbitrary, Gen, Properties}
import scalaz.{Equal, ValidationNel}
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties.applicative
import scalaz.std.anyVal._
import scalaz.std.tuple._
import scalaz.std.vector._

object ValidatedProp extends Properties("Validated") {
  implicit val eqOp: Equal[Op] = Equal.equalA

  implicit val arbOp: Arbitrary[Op] = Arbitrary(Gen.oneOf(
    Arbitrary.arbitrary[Int].map(Op.ArrayIndex(_)),
    Arbitrary.arbitrary[String].map(Op.DownField(_)),
    Arbitrary.arbitrary[Boolean].map(Op.TypeValue(_))))

  implicit def eqValidated[A: Equal]: Equal[Validated[Unit, A]] = Equal.equalBy(_.run(Vector()))
    // TODO
    /*new Equal[Validated[Unit, A]] {
      def equal(v1: Validated[Unit, A], v2: Validated[Unit, A]): Boolean = {
        val ops = Arbitrary.arbitrary[Vector[Op]].sample.getOrElse(Vector())
        Equal[ValidationNel[Unit, (Vector[Op], A)]].equal(v1.run(ops), v2.run(ops))
      }
    }*/

  implicit def arbValidated[A: Arbitrary]: Arbitrary[Validated[Unit, A]] =
    Arbitrary(Arbitrary.arbitrary[ValidationNel[Unit, (Vector[Op], A)]].map(v => Validated(_ => v)))

  include(applicative.laws[Validated[Unit, ?]])
}
