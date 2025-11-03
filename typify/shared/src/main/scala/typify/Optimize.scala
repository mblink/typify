package typify

import formless.hlist.HList

trait TWitness[T] { final type Out = T }

class Optimize[L, P](val tp: Typify[L, P]) {
  def folder[G <: HList, R <: HList](@annotation.nowarn("msg=(unused|never used)") in: G)(
    implicit rf: PVFolder[P, L, G, R]
  ): PVFolder[P, L, G, R] = rf

  def success[G <: HList, R <: HList](@annotation.nowarn("msg=(unused|never used)") in: G)(
    implicit @annotation.nowarn("msg=(unused|never used)") rf: PVFolder[P, L, G, R]
  ): TWitness[R] = new TWitness[R] {}
}
