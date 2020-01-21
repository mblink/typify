import cats.data.ValidatedNel

package object typify extends typify.StringOps {
  type PV[P, L, A] = Cursor[P] => ValidatedNel[L, A]
  type KPV[P, L, A] = String => PV[P, L, A]

  type E2L[L, P] = (Cursor[P], ParseError) => L

  private[typify] def ap[A, B](a: A)(f: A => B): B = f(a)
}
