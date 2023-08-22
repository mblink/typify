package typify.tuple

/**
 * Type class supporting left scanning of this `Tuple` with a binary polymorphic function.
 */
trait LeftScanner[L, In, F] extends DepFn2[L, In] with Serializable

object LeftScanner{
  type Aux[L, In, F, O] = LeftScanner[L, In, F] { type Out = O }

  inline def apply[L, In, F](using s: LeftScanner[L, In, F]): LeftScanner.Aux[L, In, F, s.Out] = s

  given emptyTupleLeftScanner[In, F]: LeftScanner.Aux[EmptyTuple, In, F, In *: EmptyTuple] =
    new LeftScanner[EmptyTuple, In, F] {
      type Out = In *: EmptyTuple
      def apply(l: EmptyTuple, in: In) = in *: EmptyTuple
    }

  given tupleNLeftScanner[H, T <: Tuple, In, F, OutP, ScanOut <: Tuple](
    using ch: Case2.Aux[F, H, In, OutP],
    st: LeftScanner.Aux[T, OutP, F, ScanOut],
  ): LeftScanner.Aux[H *: T, In, F, In *: ScanOut] =
    new LeftScanner[H *: T, In, F] {
      type Out = In *: ScanOut
      def apply(l: H *: T, in: In) = in *: st(l.tail, ch(l.head, in))
    }
}
