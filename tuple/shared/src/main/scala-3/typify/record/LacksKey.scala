package typify.record

import scala.util.NotGiven

opaque type LacksKey[T <: Tuple, K] = NotGiven[Selector[T, K]]

object LacksKey {
  inline def apply[T <: Tuple, K]: LacksKey[T, K] =
    compiletime.summonInline[NotGiven[Selector[T, K]]]

  inline given inst[T <: Tuple, K]: LacksKey[T, K] =
    apply[T, K]
}
