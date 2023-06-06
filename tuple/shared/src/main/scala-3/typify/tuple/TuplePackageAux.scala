package typify.tuple

trait TuplePackageAux {
  final type Tuple = scala.Tuple
  final type *:[H, T <: Tuple] = scala.*:[H, T]
  final val *: : scala.*:.type = scala.*:
  final type EmptyTuple = scala.EmptyTuple
  final val EmptyTuple: EmptyTuple = scala.EmptyTuple

  extension[T <: Tuple](t: T) {
    final def :::[P <: Tuple](p: P)(using pr: Prepend[P, T]): pr.Out = pr(p, t)

    final def toList[Lub](implicit tl: ToList[T, Lub]): List[Lub] = tl(t)

    final def foldRight[A](a: A)(op: Poly)(implicit f: RightFolder[T, A, op.type]): f.Out = f(t, a)

    final def mapPoly(f: Poly)(implicit m: Mapper[f.type, T]): m.Out = m(t)
  }
}
