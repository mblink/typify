package typify.tuple

/**
 * Type class supporting left-folding a polymorphic binary function over this `Tuple`.
 */
trait LeftFolder[L <: Tuple, In, F] extends DepFn2[L, In]

object LeftFolder {
  type Aux[L <: Tuple, In, F, O] = LeftFolder[L, In, F] { type Out = O }

  inline def apply[L <: Tuple, In, F](using f: LeftFolder[L, In, F]): LeftFolder.Aux[L, In, F, f.Out] = f

  given leftFolderEmptyTuple[In, F]: LeftFolder.Aux[EmptyTuple, In, F, In] =
    new LeftFolder[EmptyTuple, In, F] {
      type Out = In
      def apply(l: EmptyTuple, in: In): Out = in
    }

  given leftFolderTupleCons[H, T <: Tuple, In, F, OutH, FtOut](
    using f: Case2[F, In, H, OutH],
    ft: LeftFolder.Aux[T, OutH, F, FtOut],
  ): LeftFolder.Aux[H *: T, In, F, FtOut] =
    new LeftFolder[H *: T, In, F] {
      type Out = FtOut
      def apply(l: H *: T, in: In): Out = ft(l.tail, f.run(in, l.head))
    }
}
