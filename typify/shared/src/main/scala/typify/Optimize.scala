package typify

import formless.tuple.Tuple

trait TWitness[T] { final type Out = T }

class Optimize[L, P](val tp: Typify[L, P]) {
  def folder[G <: Tuple, R <: Tuple](@deprecated("unused", "") in: G)(
    implicit rf: PVFolder[P, L, G, R]
  ): PVFolder[P, L, G, R] = rf

  def success[G <: Tuple, R <: Tuple](@deprecated("unused", "")  in: G)(
    implicit @deprecated("unused", "") rf: PVFolder[P, L, G, R]
  ): TWitness[R] = new TWitness[R] {}
}
