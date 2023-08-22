package typify.tuple

type ZipWithIndexT[L <: Tuple] = ZipWithIndexT0[L, 0]

type ZipWithIndexT0[L <: Tuple, I <: Int] <: Tuple = L match {
  case EmptyTuple => EmptyTuple
  case h *: t => (h, I) *: ZipWithIndexT0[t, compiletime.ops.int.S[I]]
}

/**
 * Type class supporting zipping a `Tuple` with its element indices, resulting in a `Tuple` of `Tuple2`s of the form
 * ({element from input tuple}, {element index})
 */
trait ZipWithIndex[L] extends DepFn1[L] with Serializable

object ZipWithIndex {
  type Aux[L, O] = ZipWithIndex[L] { type Out = O }

  inline def apply[L](using z: ZipWithIndex[L]): ZipWithIndex.Aux[L, z.Out] = z

  given tupleZipWithIndex[L <: Tuple]: ZipWithIndex.Aux[L, ZipWithIndexT[L]] =
    new ZipWithIndex[L] {
      type Out = ZipWithIndexT[L]
      def apply(l: L): Out = Tuple.fromArray(l.toArray.zipWithIndex).asInstanceOf[Out]
    }
}
