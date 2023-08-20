package typify.tuple

/**
 * Type class supporting right-folding a polymorphic binary function over this `Tuple`.
 */
trait RightFolder[L <: Tuple, In, F] extends DepFn2[L, In]

object RightFolder {
  type Aux[L <: Tuple, In, F, O] = RightFolder[L, In, F] { type Out = O }

  inline def apply[L <: Tuple, In, F](using f: RightFolder[L, In, F]): RightFolder.Aux[L, In, F, f.Out] = f

  given rightFolderEmptyTuple[In, F]: RightFolder.Aux[EmptyTuple, In, F, In] =
    new RightFolder[EmptyTuple, In, F] {
      type Out = In
      def apply(l: EmptyTuple, in: In): Out = in
    }

  given rightFolderTupleCons[H, T <: Tuple, In, F, OutT, FOut](
    using ft: RightFolder.Aux[T, In, F, OutT],
    f: Case2[F, H, OutT, FOut],
  ): RightFolder.Aux[H *: T, In, F, FOut] =
    new RightFolder[H *: T, In, F] {
      type Out = FOut
      def apply(l: H *: T, in: In): Out = f.run(l.head, ft(l.tail, in))
    }
}
