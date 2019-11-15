package typify

import org.scalacheck.{Arbitrary, Gen, Properties}
import scalaz.{Applicative, Equal, NonEmptyList, State, Validation}
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.anyVal._
import scalaz.std.option._

object ValidationTProp extends Properties("ValidationT") {
  type ValidationTOpt[E, A] = ValidationT[Option, E, A]
  type ValidationTOptInt[A] = ValidationTOpt[NonEmptyList[Int], A]

  implicit val eqOp: Equal[Op] = Equal.equalA

  implicit val arbOp: Arbitrary[Op] = Arbitrary(Gen.oneOf(
    Arbitrary.arbitrary[Int].map(Op.ArrayIndex(_)),
    Arbitrary.arbitrary[String].map(Op.DownField(_)),
    Arbitrary.arbitrary[Boolean].map(Op.TypeValue(_))))

  private lazy val ops = Arbitrary.arbitrary[Vector[Op]].sample.getOrElse(Vector())

  implicit def stateEqual[A](implicit A: Equal[(Vector[Op], A)]): Equal[State[Vector[Op], A]] = A.contramap(_.run(ops))

  implicit def validationTArbitrary[F[_], E, A](implicit F: Applicative[F], E: Arbitrary[E], A: Arbitrary[A]): Arbitrary[ValidationT[F, E, A]] =
    Arbitrary(Arbitrary.arbitrary[Validation[E, A]].map(v => ValidationT(F.point(v))))

  include(functor.laws[ValidationTOptInt])
  include(foldable.laws[ValidationTOptInt])
  include(apply.laws[ValidationTOptInt])
  include(traverse.laws[ValidationTOptInt])
  include(applicative.laws[ValidationTOptInt])
  // include(monad.laws[ValidationTOptInt])
  include(bifunctor.laws[ValidationTOpt])
  include(bitraverse.laws[ValidationTOpt])
  include(equal.laws[ValidationTOptInt[Int]])
  include(semigroup.laws[ValidationTOptInt[Int]])
}
