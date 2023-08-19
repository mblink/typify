package typify.tuple

trait ZipOne[H, T] extends DepFn2[H, T]

sealed trait ZipOneLP {
  final type Aux[H, T, O] = ZipOne[H, T] { type Out = O }

  final given zipOne1[H]: ZipOne.Aux[H, EmptyTuple, EmptyTuple] =
    new ZipOne[H, EmptyTuple] {
      type Out = EmptyTuple
      def apply(h: H, t: EmptyTuple): Out = EmptyTuple
    }

  final given zipOne2[T]: ZipOne.Aux[EmptyTuple, T, EmptyTuple] =
    new ZipOne[EmptyTuple, T] {
      type Out = EmptyTuple
      def apply(h: EmptyTuple, t: T): Out = EmptyTuple
    }

  final given zipOne4[HH, HT <: Tuple, TH <: Tuple, TT <: Tuple, ZotOut <: Tuple](
    using zot: ZipOne.Aux[HT, TT, ZotOut],
  ): ZipOne.Aux[HH *: HT, TH *: TT, (HH *: TH) *: ZotOut] =
    new ZipOne[HH *: HT, TH *: TT] {
      type Out = (HH *: TH) *: ZotOut
      def apply(h: HH *: HT, t: TH *: TT): Out = (h.head *: t.head) *: zot(h.tail, t.tail)
    }
}

object ZipOne extends ZipOneLP {
  inline def apply[H <: Tuple, T <: Tuple](implicit z: ZipOne[H, T]): ZipOne.Aux[H, T, z.Out] = z

  given zipOne0: ZipOne.Aux[EmptyTuple, EmptyTuple, EmptyTuple] =
    new ZipOne[EmptyTuple, EmptyTuple] {
      type Out = EmptyTuple
      def apply(h: EmptyTuple, t: EmptyTuple): Out = EmptyTuple
    }

  given zipOne3[H, T <: Tuple]: ZipOne.Aux[H *: EmptyTuple, T *: EmptyTuple, (H *: T) *: EmptyTuple] =
    new ZipOne[H *: EmptyTuple, T *: EmptyTuple] {
      type Out = (H *: T) *: EmptyTuple
      def apply(h: H *: EmptyTuple, t: T *: EmptyTuple): Out = (h.head *: t.head) *: EmptyTuple
    }
}

