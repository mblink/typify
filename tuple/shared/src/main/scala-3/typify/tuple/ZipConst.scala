package typify.tuple

type ZipConstT[C, L <: Tuple] <: Tuple = L match {
  case EmptyTuple => EmptyTuple
  case h *: t => (h, C) *: ZipConstT[C, t]
}

trait ZipConst[C, L] extends DepFn2[C, L]

object ZipConst {
  type Aux[C, L, O] = ZipConst[C, L] { type Out = O }

  inline def apply[C, L](using z: ZipConst[C, L]): ZipConst.Aux[C, L, z.Out] = z

  private def zipConst[C, T <: Tuple](c: C, t: T): ZipConstT[C, T] =
    t match {
      case _: EmptyTuple => EmptyTuple
      case x: (h *: t) => (x.head, c) *: zipConst[C, t](c, x.tail)
    }

  given tupleZipConst[C, L <: Tuple]: ZipConst.Aux[C, L, ZipConstT[C, L]] =
    new ZipConst[C, L] {
      type Out = ZipConstT[C, L]
      def apply(c: C, l: L): Out = zipConst[C, L](c, l)
    }
}
