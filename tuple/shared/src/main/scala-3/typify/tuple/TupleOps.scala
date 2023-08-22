package typify.tuple

final class ReinsertAux[L, O](private val l: L) extends AnyVal {
  final def apply[U](u: U)(using r: Remove[O, U], ev: (U, L) =:= r.Out): O = r.reinsert((u, l))
}

final class ReinsertAllAux[L, O](private val l: L) extends AnyVal {
  final def apply[SL](sl: SL)(using r: RemoveAll.Aux[O, SL, (SL, L)]): O = r.reinsert((sl, l))
}

final class ReplaceTypeAux[L, U](private val l: L) extends AnyVal {
  final def apply[V](v: V)(using r: Replacer[L, U, V]): r.Out = r(l, v)
}

final class UpdatedTypeAux[L, U](private val l: L) extends AnyVal {
  final def apply[V, Out <: Tuple](v: V)(using r: Replacer.Aux[L, U, V, (U, Out)]): Out = r(l, v)._2
}

final class UpdatedAtAux[L, N](private val l: L) extends AnyVal {
  final def apply[U, V, Out <: Tuple](u: U)(using r: ReplaceAt.Aux[L, N, U, (V, Out)]): Out = r(l, u)._2
}

final class PatchAux[L, N, M](private val l: L) extends AnyVal {
  final def apply[In <: Tuple](in: In)(using p: Patcher[N, M, L, In]): p.Out = p(l, in)
}

final class TypifyTupleOps[L <: Tuple](private val l: L) extends AnyVal {
  /**
   * Append the argument element to this `Tuple`.
   */
  final def :+[T](t: T)(using p: Prepend[L, T *: Tuple]): p.Out = p(l, t *: EmptyTuple)

  /**
   * Append the argument `Tuple` to this `Tuple`.
   */
  final def ++[S <: Tuple](suffix: S)(using p: Prepend[L, S]): p.Out = p(l, suffix)

  /**
   * Prepend the argument `Tuple` to this `Tuple`.
   */
  final def ++:[P <: Tuple](prefix: P)(using p: Prepend[P, L]): p.Out = p(prefix, l)

  /**
   * Prepend the argument `Tuple` to this `Tuple`.
   */
  final def :::[P <: Tuple](prefix: P)(using p: Prepend[P, L]): p.Out = p(prefix, l)

  /**
   * Prepend the reverse of the argument `Tuple` to this `Tuple`.
   */
  final def reverse_:::[P <: Tuple](prefix: P)(using p: ReversePrepend[P, L]): p.Out = p(prefix, l)

  /**
   * Returns the `N`th element of this `Tuple`. An explicit type argument must be provided. Available only if there is
   * evidence that this `Tuple` has at least `N` elements.
   */
  final def apply[N <: Int](using n: ValueOf[N]): Tuple.Elem[L, N] = l.productElement(n.value).asInstanceOf[Tuple.Elem[L, N]]

  /**
   * Returns the `N`th element of this `Tuple`. Available only if there is evidence that this `Tuple` has at least `N`
   * elements.
   */
  final def apply[N <: Int](n: N): Tuple.Elem[L, N] = l.productElement(n).asInstanceOf[Tuple.Elem[L, N]]

  /**
   * Returns the `N`th element of this `Tuple`. An explicit type argument must be provided. Available only if there is
   * evidence that this `Tuple` has at least `N` elements.
   */
  final def at[N <: Int](using n: ValueOf[N]): Tuple.Elem[L, N] = l.productElement(n.value).asInstanceOf[Tuple.Elem[L, N]]

  /**
   * Returns the `N`th element of this `Tuple`. Available only if there is evidence that this `Tuple` has at least `N`
   * elements.
   */
  final def at[N <: Int](n: N): Tuple.Elem[L, N] = l.productElement(n).asInstanceOf[Tuple.Elem[L, N]]

  /**
   * Returns the last element of this `Tuple`. Available only if there is evidence that this `Tuple` is composite.
   */
  final def last(using lst: Last[L]): lst.Out = lst(l)

  /**
   * Returns a `Tuple` consisting of all the elements of this `Tuple` except the last. Available only if there is
   * evidence that this `Tuple` is composite.
   */
  final def init(using i: Init[L]): i.Out = i(l)

  /**
   * Returns the first element of type `U` of this `Tuple`. An explicit type argument must be provided. Available only
   * if there is evidence that this `Tuple` has an element of type `U`.
   */
  final def select[U](using s: Selector[L, U]): U = s(l)

  final def selectMany[Ids <: Tuple](using s: SelectMany[L, Ids]): s.Out = s(l)

  // TODO - this doesn't work well because e.g. `0 *: EmptyTuple` is typed as `Int *: EmptyTuple`
  // instead of a preserving the singleton type of `0 *: EmptyTuple`
  final def selectMany[Ids <: Tuple](ids: Ids)(using s: SelectMany[L, Ids]): s.Out = s(l)

  /**
   * Returns the elements of this `Tuple` specified by the range of ids in [A,B[
   * Available only if there is evidence that this `Tuple` contains all elements in that range
   */
  final def selectRange[A <: Int, B <: Int](using s: SelectRange[L,A,B]): s.Out = s(l)

  final def selectRange(a: Int, b: Int)(using s: SelectRange[L, a.type, b.type]): s.Out = s(l)

  /**
   * Returns all elements of type `U` of this `Tuple`. An explicit type argument must be provided.
   */
  final def filter[U](using p: Partition[L, U]): p.Prefix  = p.filter(l)

  /**
   * Returns all elements of type different than `U` of this `Tuple`. An explicit type argument must be provided.
   */
  final def filterNot[U](using p: Partition[L, U]): p.Suffix  = p.filterNot(l)

  final def partition[U](using p: Partition[L, U]): (p.Prefix, p.Suffix) = p(l)

  /**
   * Returns the first element of type `U` of this `Tuple` plus the remainder of the `Tuple`. An explicit type argument
   * must be provided. Available only if there is evidence that this `Tuple` has an element of type `U`.
   *
   * The `Elem` suffix is here to avoid creating an ambiguity with RecordOps#remove and should be removed if
   * SI-5414 is resolved in a way which eliminates the ambiguity.
   */
  final def removeElem[U](using r: Remove[L, U]): r.Out = r(l)

  /**
   * Returns the first elements of this `Tuple` that have types in `SL` plus the remainder of the `Tuple`. An expicit
   * type argument must be provided. Available only if there is evidence that this `Tuple` contains elements with
   * types in `SL`.
   */
  final def removeAll[SL <: Tuple](using r: RemoveAll[L, SL]): r.Out = r(l)

  /**
   * Reinserts an element `U` into this `Tuple` to return another `Tuple` `O`.
   */
  final def reinsert[O]: ReinsertAux[L, O] = new ReinsertAux[L, O](l)

  /**
   * Reinserts the elements of `SL` into this `Tuple` to return another `Tuple` `O`.
   */
  final def reinsertAll[O]: ReinsertAllAux[L, O] = new ReinsertAllAux[L, O](l)

  /**
   * Replaces the first element of type `U` of this `Tuple` with the supplied value, also of type `U` returning both
   * the replaced element and the updated `Tuple`. Available only if there is evidence that this `Tuple` has an element
   * of type `U`.
   */
  final def replace[U](u: U)(using r: Replacer[L, U, U]): r.Out = r(l, u)

  /**
   * Replaces the first element of type `U` of this `Tuple` with the supplied value of type `V`, returning both the
   * replaced element and the updated `Tuple`. An explicit type argument must be provided for `U`. Available only if
   * there is evidence that this `Tuple` has an element of type `U`.
   */
  final def replaceType[U]: ReplaceTypeAux[L, U] = new ReplaceTypeAux[L, U](l)

  /**
   * Replaces the first element of type `U` of this `Tuple` with the supplied value, also of type `U`. Available only
   * if there is evidence that this `Tuple` has an element of type `U`.
   *
   * The `Elem` suffix is here to avoid creating an ambiguity with RecordOps#updated and should be removed if
   * SI-5414 is resolved in a way which eliminates the ambiguity.
   */
  final def updatedElem[U, Out <: Tuple](u: U)(using r: Replacer.Aux[L, U, U, (U, Out)]): Out = r(l, u)._2

  /**
   * Replaces the first element of type `U` of this `Tuple` with the result of its transformation to a `V` via the
   * supplied function. Available only if there is evidence that this `Tuple` has an element of type `U`.
   */
  final def updateTypeWith[U, V, Out <: Tuple](f: U => V)(using m: Modifier.Aux[L, U, V, (U, Out)]): Out = m(l, f)._2

  /**
   * Replaces the `N`th element of this `Tuple` with the result of calling the supplied function on it.
   * Available only if there is evidence that this `Tuple` has `N` elements.
   */
  def updateAtWith[V](n: Int)(f: Tuple.Elem[L, n.type] => V)(
    using m: ModifierAt[L, n.type, Tuple.Elem[L, n.type], V],
  ): m.Out = m(l, f)

  /**
   * Replaces the first element of type `U` of this `Tuple` with the supplied value of type `V`. An explicit type
   * argument must be provided for `U`. Available only if there is evidence that this `Tuple` has an element of
   * type `U`.
   */
  final def updatedType[U]: UpdatedTypeAux[L, U] = new UpdatedTypeAux[L, U](l)

  /**
   * Replaces the `N`th element of this `Tuple` with the supplied value of type `U`. An explicit type argument
   * must be provided for `N`. Available only if there is evidence that this `Tuple` has at least `N` elements.
   */
  final def updatedAt[N]: UpdatedAtAux[L, N] = new UpdatedAtAux[L, N](l)

  /**
   * Replaces the `n`th element of this `Tuple` with the supplied value of type `U`. Available only if there is
   * evidence that this `Tuple` has at least `n` elements.
   */
  final def updatedAt[U, V, Out <: Tuple](n: Int, u: U)(using r: ReplaceAt.Aux[L, n.type, U, (V, Out)]): Out = r(l, u)._2

  /**
   * Returns the first `N` elements of this `Tuple`. An explicit type argument must be provided. Available only if
   * there is evidence that this `Tuple` has at least `N` elements.
   */
  final def take[N](using t: Take[L, N]): t.Out = t(l)

  /**
   * Returns the first `n` elements of this `Tuple`. Available only if there is evidence that this `Tuple` has at
   * least `n` elements.
   */
  final def take(n: Int)(using t: Take[L, n.type]): t.Out = t(l)

  /**
   * Returns all but the  first `N` elements of this `Tuple`. An explicit type argument must be provided. Available
   * only if there is evidence that this `Tuple` has at least `N` elements.
   */
  final def drop[N](using d: Drop[L, N]): d.Out = d(l)

  /**
   * Returns all but the  first `n` elements of this `Tuple`. Available only if there is evidence that this `Tuple`
   * has at least `n` elements.
   */
  final def drop(n: Int)(using d: Drop[L, n.type]): d.Out = d(l)

  /**
   * Splits this `Tuple` at the `N`th element, returning the prefix and suffix as a pair. An explicit type argument
   * must be provided. Available only if there is evidence that this `Tuple` has at least `N` elements.
   */
  final def split[N](using s: Split[L, N]): s.Out = s(l)

  /**
   * Splits this `Tuple` at the `n`th element, returning the prefix and suffix as a pair. Available only if there is
   * evidence that this `Tuple` has at least `n` elements.
   */
  final def split(n: Int)(using s: Split[L, n.type]): s.Out = s(l)

  /**
   * Splits this `Tuple` at the `N`th element, returning the reverse of the prefix and suffix as a pair. An explicit
   * type argument must be provided. Available only if there is evidence that this `Tuple` has at least `N` elements.
   */
  final def reverse_split[N](using s: ReverseSplit[L, N]): s.Out = s(l)

  /**
   * Splits this `Tuple` at the `n`th element, returning the reverse of the prefix and suffix as a pair. Available
   * only if there is evidence that this `Tuple` has at least `n` elements.
   */
  final def reverse_split(n: Int)(using s: ReverseSplit[L, n.type]): s.Out = s(l)

  /**
   * Splits this `Tuple` at the first occurrence of an element of type `U`, returning the prefix and suffix as a pair.
   * An explicit type argument must be provided. Available only if there is evidence that this `Tuple` has an element
   * of type `U`.
   */
  final def splitLeft[U](using s: SplitLeft[L, U]): s.Out = s(l)

  /**
   * Splits this `Tuple` at the first occurrence of an element of type `U`, returning reverse of the prefix and suffix
   * as a pair. An explicit type argument must be provided. Available only if there is evidence that this `Tuple` has
   * an element of type `U`.
   */
  final def reverse_splitLeft[U](using s: ReverseSplitLeft[L, U]): s.Out = s(l)

  /**
   * Splits this `Tuple` at the last occurrence of an element of type `U`, returning the prefix and suffix as a pair.
   * An explicit type argument must be provided. Available only if there is evidence that this `Tuple` has an element
   * of type `U`.
   */
  final def splitRight[U](using s: SplitRight[L, U]): s.Out = s(l)

  /**
   * Splits this `Tuple` at the last occurrence of an element of type `U`, returning reverse of the prefix and suffix
   * as a pair. An explicit type argument must be provided. Available only if there is evidence that this `Tuple` has
   * an element of type `U`.
   */
  final def reverse_splitRight[U](using s: ReverseSplitRight[L, U]): s.Out = s(l)

  /**
   * Permutes this `Tuple` into the same order as another `Tuple`. An explicit type argument must be supplied.
   * Available only if both `Tuple`s have elements of the same types.
   */
  final def align[M <: Tuple](using a: Align[L, M]): M = a(l)

  /**
   * Permutes this `Tuple` into the same order as the supplied `Tuple` with the same element types. Available only if
   * both `Tuple`s have elements of the same types.
   */
  final def align[M <: Tuple](m: M)(using a: Align[L, M]): M = a(l)

  /**
   * Reverses this `Tuple`.
   */
  final def reverse(using r: Reverse[L]): r.Out = r(l)

  /**
   * Maps a higher rank function across this `Tuple`.
   */
  final def mapPoly(f: Poly)(using m: Mapper[f.type, L]): m.Out = m(l)

  /**
   * Flatmaps a higher rank function across this `Tuple`.
   */
  final def flatMap(f: Poly)(using m: FlatMapper[f.type, L]): m.Out = m(l)

  /**
   * Conses an element onto each row of this HMatrix (Tuple of Tuples).
   */
  final def mapCons[A](a: A)(using m: MapCons[A, L]): m.Out = m(a, l)

  /**
   * Replaces each element of this `Tuple` with a constant value.
   */
  final def mapConst[C](c: C)(using m: ConstMapper[C, L]): m.Out = m(c, l)

  /**
   * Collect a higher rank function across this `Tuple`.
   */
  final def collect(p: Poly)(using c: Collect[L, p.type]): c.Out = c(l)

  /**
   * Maps a higher rank function `f` across this `Tuple` and folds the result using monomorphic combining operator
   * `op`. Available only if there is evidence that the result type of `f` at each element conforms to the argument
   * type of `op`.
   */
  final def foldMap[R](z: R)(f: Poly)(op: (R, R) => R)(using folder: MapFolder[L, R, f.type]): R = folder(l, z, op)

  /**
   * Computes a left fold over this `Tuple` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the partial results of the appropriate types.
   */
  final def foldLeft[R](z: R)(op: Poly)(using folder: LeftFolder[L, R, op.type]): folder.Out = folder(l, z)

  /**
   * Computes a right fold over this `Tuple` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the partial results of the appropriate types.
   */
  final def foldRight[A](a: A)(op: Poly)(using f: RightFolder[L, A, op.type]): f.Out = f(l, a)

  /**
   * Computes a left reduce over this `Tuple` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence that this `Tuple` has at least one element and that `op` can consume/produce all the partial
   * results of the appropriate types.
   */
  final def reduceLeft(op: Poly)(using r: LeftReducer[L, op.type]): r.Out = r(l)

  /**
   * Computes a right reduce over this `Tuple` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence that this `Tuple` has at least one element and that `op` can consume/produce all the partial
   * results of the appropriate types.
   */
  final def reduceRight(op: Poly)(using r: RightReducer[L, op.type]): r.Out = r(l)

  /**
    * Repeats this `Tuple` N times.
    */
  final def repeat[N](using r: Repeat[L, N]): r.Out = r(l)

  /**
   * Zips this `Tuple` of monomorphic function values with its argument `Tuple` of correspondingly typed function
   * arguments returning the result of each application as a `Tuple`. Available only if there is evidence that the
   * corresponding function and argument elements have compatible types.
   */
  final def zipApply[A <: Tuple](a: A)(using z: ZipApply[L, A]): z.Out = z(l, a)

  /**
   * Zips this `Tuple` with its argument `Tuple` of `Tuple`s, returning a `Tuple` of `Tuple`s with each element of
   * this `Tuple` prepended to the corresponding `Tuple` element of the argument `Tuple`.
   */
  final def zipOne[T <: Tuple](t: T)(using z: ZipOne[L, T]): z.Out = z(l, t)

  /**
   * Zips this `Tuple` with a constant, resulting in a `Tuple` of tuples of the form
   * ({element from this `Tuple`}, {supplied constant})
   */
  final def zipConst[C](c: C)(using z: ZipConst[C, L]): z.Out = z(c, l)

  /**
   * Zips this `Tuple` with its argument `Tuple` using argument `Poly2`, returning an `Tuple`.
   * Doesn't require this to be the same length as its `Tuple` argument, but does require evidence that its
   * `Poly2` argument is defined at their intersection.
   */
  final def zipWith[R <: Tuple, P <: Poly2](r: R)(p: P)(using z: ZipWith[L, R, P]): z.Out = z(l, r)

  /**
   * Zips this `Tuple` with its element indices,  resulting in a `Tuple` of  tuples of the form
   * ({element from input tuple}, {element index})
   */
  final def zipWithIndex(using z: ZipWithIndex[L]): z.Out = z(l)

  /**
   * Transposes this `Tuple`.
   */
  final def transpose(using t: Transposer[L]): t.Out = t(l)

  /**
   * Returns a `Tuple` typed as a repetition of the least upper bound of the types of the elements of this `Tuple`.
   */
  final def unify(using u: Unifier[L]): u.Out = u(l)

  /**
   * Returns a `Tuple` with all elements that are subtypes of `B` typed as `B`.
   */
  final def unifySubtypes[B](using u: SubtypeUnifier[L, B]): u.Out = u(l)

  /**
   * Compute the length of this `Tuple`.
   */
  final def length(using len: Length[L]): len.Out = len()

  /**
   * Compute the length of this `Tuple` as a runtime Int value.
   */
  final def runtimeLength: Int = {
    @annotation.tailrec def loop(l: Tuple, acc: Int): Int = l match {
      case EmptyTuple => acc
      case _ *: t => loop(t, acc + 1)
    }

    loop(l, 0)
  }

  /**
   * Convert this `Tuple` to a `List[Any]`.
   */
  final def runtimeList: List[Any] = {
    val builder = List.newBuilder[Any]

    @annotation.tailrec def loop(l: Tuple): Unit = l match {
      case EmptyTuple => ()
      case h *: t =>
        builder += h
        loop(t)
    }

    loop(l)
    builder.result()
  }

  /**
   * Converts this `Tuple` to a `M` of elements typed as the least upper bound of the types of the elements
   * of this `Tuple`.
   */
  final def to[M[_]](using t: ToTraversable[L, M]): t.Out = t(l)

  /**
   * Converts this `Tuple` to a `M` of elements typed as the least upper bound of the types of the elements
   * of this `Tuple`.
   */
  final def toLub[M[_], Lub](using t: ToTraversable.Aux[L, M, Lub]): t.Out = t(l)

  /**
   * Converts this `Tuple` to an ordinary `List` of elements typed as the least upper bound of the types of the elements
   * of this `Tuple`.
   */
  final def toListLub[Lub](using tl: ToList[L, Lub]): tl.Out = tl(l)

  /**
   * Converts this `Tuple` to an `Array` of elements typed as the least upper bound of the types of the elements
   * of this `Tuple`.
   *
   * It is advisable to specify the type parameter explicitly, because for many reference types, case classes in
   * particular, the inferred type will be too precise (ie. `Product with Serializable with CC` for a typical case class
   * `CC`) which interacts badly with the invariance of `Array`s.
   */
  final def toArrayLub[Lub](using ta: ToArray[L, Lub]): ta.Out = ta(l)

  /**
   * Displays all elements of this Tuple in a string using start, end, and separator strings.
    */
  final def mkString(start: String, sep: String, end: String): String = {
    @annotation.tailrec
    def go(acc: String, sub: Tuple): String = sub match {
      case EmptyTuple => ""
      case h *: EmptyTuple => acc ++ h.toString
      case h *: t => go(acc ++ h.toString + sep, t)
    }

    go(start, l) + end
  }


  /**
   * Converts this `Tuple` of values into a record with the provided keys.
   */
  final def zipWithKeys[K <: Tuple](keys: K)(using z: ZipWithKeys[K, L]): z.Out = z(l)

  /**
   * Converts this `Tuple` of values into a record with given keys. A type argument must be provided.
   */
  final def zipWithKeys[K <: Tuple](using z: ZipWithKeys[K, L]): z.Out = z(l)

  /**
   * Returns all permutations of this `Tuple`
   */
  final def permutations(using p: Permutations[L]): p.Out = p(l)

  /**
   * Rotate this `Tuple` left by N. An explicit type argument must be provided.
   */
  final def rotateLeft[N](using r: RotateLeft[L, N]): r.Out = r(l)

  /**
   * Rotate this `Tuple` left by N
   */
  final def rotateLeft(n: Int)(using r: RotateLeft[L, n.type]): r.Out = r(l)

  /**
   * Rotate this `Tuple` right by N. An explicit type argument must be provided.
   */
  final def rotateRight[N](using r: RotateRight[L, N]): r.Out = r(l)

  /**
   * Rotate this `Tuple` right by N
   */
  final def rotateRight(n: Int)(using r: RotateRight[L, n.type]): r.Out = r(l)

  /**
   * Computes a left scan over this `Tuple` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the results of the appropriate types.
   */
  final def scanLeft[A](z: A)(op: Poly)(using s: LeftScanner[L, A, op.type]): s.Out = s(l, z)

  /**
   * Computes a right scan over this `Tuple` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the results of the appropriate types.
   */
  final def scanRight[A](z: A)(op: Poly)(using s: RightScanner[L, A, op.type]): s.Out = s(l, z)

  /**
   *
   * Produces a new `Tuple` where a slice of this `Tuple` is replaced by another. Available only if there are at least
   * ``n`` plus ``m`` elements.
   */
  final def patch[In <: Tuple](n: Int, in: In, m: Int)(using p: Patcher[n.type, m.type, L, In]): p.Out = p(l, in)

  /**
   * Produces a new `Tuple` where a slice of this `Tuple` is replaced by another. Two explicit type arguments must be
   * provided. Available only if there are at least `N` plus `M` elements.
   */
  final def patch[N, M]: PatchAux[L, N, M] = new PatchAux[L, N, M](l)

  /**
   * Finds the first element of the Tuple for which the given Poly is defined, and applies the Poly to it.
   */
  final def collectFirst[P <: Poly](p: P)(using c: CollectFirst[L, p.type]): c.Out = c(l)

  /**
   * Groups the elements of this `Tuple` into tuples of `n` elements, offset by `step`
   */
  final def group(n: Int, step: Int)(using g: Grouper[L, n.type, step.type]): g.Out = g(l)

  /**
   * Appends `elem` until a given length `N` is reached.
   */
  final def padTo[A](n: Int, elem: A)(using p: PadTo[n.type, A, L]): p.Out = p(elem, l)

  /**
   * Slices beginning at index `from` and afterwards, up until index `until`
   */
  final def slice(from: Int, until: Int)(using s: Slice[from.type, until.type, L]): s.Out = s(l)

  /**
   * Returns all combinations of exactly length `N` of elements from this `Tuple`
   */
  final def combinations(n: Int)(using c: Combinations[n.type, L]): c.Out = c(l)
}
