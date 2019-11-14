package object typify {
  type E2L[L, P] = (Parsed[P], ParseError) => L
  type PV[P, L, A] = Parsed[P] => Validated[L, A]
  type KPV[P, L, A] = String => PV[P, L, A]
  type ParsedValidated[A] = Validated[ParseError, A]
}
