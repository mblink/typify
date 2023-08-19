package typify.tuple

trait ZipApply[FL, AL] extends DepFn2[FL, AL]

object ZipApply {
  type Aux[FL, AL, O] = ZipApply[FL, AL] { type Out = O }

  inline def apply[FL, AL](using z: ZipApply[FL, AL]): ZipApply.Aux[FL, AL, z.Out] = z

  given emptyTupleZipApply: ZipApply.Aux[EmptyTuple, EmptyTuple, EmptyTuple] =
    new ZipApply[EmptyTuple, EmptyTuple] {
      type Out = EmptyTuple
      def apply(fl : EmptyTuple, al : EmptyTuple): Out = EmptyTuple
    }

  given tupleNZipApply[T, R, FLT <: Tuple, ALT <: Tuple, ZttOut <: Tuple](
    using ztt : ZipApply.Aux[FLT, ALT, ZttOut],
  ): ZipApply.Aux[(T => R) *: FLT, T *: ALT, R *: ZttOut] =
    new ZipApply[(T => R) *: FLT, T *: ALT] {
      type Out = R *: ZttOut
      def apply(fl : (T => R) *: FLT, al : T *: ALT): Out = fl.head(al.head) *: ztt(fl.tail, al.tail)
    }
}
