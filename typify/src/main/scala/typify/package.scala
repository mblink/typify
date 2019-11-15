import scalaz.{NonEmptyList, State, ValidationNel}

package object typify {
  type Validated[L, A] = ValidationT[State[Vector[Op], ?], NonEmptyList[L], A]
  type ParsedValidated[A] = Validated[ParseError, A]

  type E2L[L, P] = (Parsed[P], ParseError) => L

  type PV[P, L, A] = Parsed[P] => (Vector[Op], ValidationNel[L, A])
  type PVWithoutOps[P, L, A] = Parsed[P] => ValidationNel[L, A]

  type KPV[P, L, A] = String => PV[P, L, A]
}
