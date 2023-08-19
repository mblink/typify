package typify.tuple

type ZipWithIndexT[L <: Tuple] = ZipWithIndexT0[L, 0]

type ZipWithIndexT0[L <: Tuple, I <: Int] <: Tuple = L match {
  case EmptyTuple => EmptyTuple
  case h *: t => (h, I) *: ZipWithIndexT0[t, compiletime.ops.int.S[I]]
}

trait ZipWithIndex[L] extends DepFn1[L]

object ZipWithIndex {
  type Aux[L, O] = ZipWithIndex[L] { type Out = O }

  inline def apply[L](using z: ZipWithIndex[L]): ZipWithIndex.Aux[L, z.Out] = z

  given tupleZipWithIndex[L <: Tuple]: ZipWithIndex.Aux[L, ZipWithIndexT[L]] =
    new ZipWithIndex[L] {
      type Out = ZipWithIndexT[L]
      def apply(l: L): Out = Tuple.fromArray(l.toArray.zipWithIndex).asInstanceOf[Out]
    }
}
