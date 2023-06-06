package typify.labelled

type ValueAtKey[T <: Tuple, K] = T match {
  case (K ->> v) *: _ => v
  case _ *: t => ValueAtKey[t, K]
}
