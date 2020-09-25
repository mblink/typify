package typify

import cats.{Functor, Id}
import cats.data.{NonEmptyList, NonEmptyVector, Validated, ValidatedNel}
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.list._
import cats.syntax.semigroup._
import cats.syntax.validated._
import scala.annotation.tailrec
import scala.language.dynamics
import shapeless.tag

/** A cursor representing a view of a specific value of type `A` in an overall value of type `T`
  *
  * @constructor Constructor
  * @tparam T The type of the overall root value
  * @tparam A The type of the current value
  * @param value The current value
  * @param lastOp The last operation performed to produce this cursor
  */
sealed abstract class GCursor[T, A](val value: A, val lastOp: CursorOp) extends Dynamic { self0 =>
  import GCursor._

  /** The type of the current focus, may differ from `A` if additional data is needed to support moving the focus */
  type Focus

  /** The type of the last cursor that produced this cursor */
  type Last <: GCursor[T, _]

  /** The root cursor focused on a value of type `T` */
  def root: GCursor.Top[T]

  /** The current focus */
  def focus: Focus

  /** The last cursor that produced this cursor */
  def last: Option[Last]

  private final type Self = GCursor.AuxL[T, A, Focus, Last]
  @inline private final val self: Self = self0

  /** The type of a new cursor produced by this cursor
    *
    * @tparam NewValue The type of the new value
    * @param NewFocus The type of the new focus
    */
  final type Next[NewValue, NewFocus] = GCursor.AuxL[T, NewValue, NewFocus, Self]

  /** The history of `CursorOp`s that led to this cursor */
  @inline final lazy val history: Vector[CursorOp] = {
    @tailrec def go(res: Vector[CursorOp], next: Option[GCursor[T, _]]): Vector[CursorOp] = next match {
      case Some(c) => go(res :+ c.lastOp, c.last)
      case None => res
    }

    go(Vector(), Some(this))
  }

  /** Create a failed `CursorHistory` */
  private final def failedHistory(failedOp: CursorOp): CursorHistory =
    CursorHistory(history, Some(NonEmptyVector.of(failedOp)))

  @inline final private def id[B](b: B): Id[B] = b
  @inline final private def dup[B](b: B): Id[(B, B)] = (b, b)

  /** Move the cursor to a new focus and value
    *
    * @tparam F[_] A functor the next cursor will be wrapped in
    * @param op The `CursorOp` representing the move being made
    * @param newVals The new focus and value wrapped in the functor `F`
    * @return The next cursor wrapped in the functor `F`
    */
  private def move[F[_]: Functor, NewFocus, NewValue](op: CursorOp)(
    newVals: F[(NewFocus, NewValue)]
  ): F[Next[NewValue, NewFocus]] =
    newVals.map { case (newFocus, newValue) => new GCursor[T, NewValue](newValue, op) {
      type Focus = NewFocus
      type Last = GCursor.AuxL[T, A, self.Focus, self.Last]

      lazy val root: GCursor.Top[T] = self.root
      lazy val focus: NewFocus = newFocus
      lazy val last: Option[GCursor.AuxL[T, A, self.Focus, self.Last]] = Some(self)
    }}

  /** If the focus is a list, move the cursor horizontally along it
    *
    * @tparam I The newly focused index in the list
    * @param op The `CursorOp` representing the move being made
    * @param getNewIdx A function from the currently focused index to the newly focused index
    * @param ev Evidence that the current focus is a `(List[A], Int)` representing the full list and the current index.
    *           This implies that the current `value` (of type `A`) is the element in the list at the given index.
    * @return An optional cursor focused on the new index, `None` if the index doesn't exist in the list.
    */
  private def moveList(
    op: CursorOp,
    newFocus: Focus => (List[A], Int),
    failedOp: CursorOp => CursorOp = identity
  ): Either[CursorHistory, Next[A, (List[A], Int)]] =
    move(op)(newFocus(focus) |> { case t @ (l, ni) =>
      Either.fromOption(l.lift(ni).map(t -> _), failedHistory(failedOp(op))) })

  private def updIdx(f: Int => Int)(implicit ev: Focus =:= (List[A], Int)): Focus => (List[A], Int) =
    ev(_) |> { case (l, i) => (l, f(i)) }

  /** Map the current value to a new type
    *
    * @tparam B The type of the new value
    * @param f A function from the current value to the new value
    * @return A new cursor focused on the value produced by the function f
    */
  final def map[B](f: A => B): Next[B, B] = move(CursorOp.WithFocus(identity[B]))(dup(f(value)))

  /** If the focus is a list, move one element to the left
    *
    * @param ev Evidence that the current focus is a `(List[A], Int)`
    * @see def moveList
    */
  final def left(implicit ev: Focus =:= (List[A], Int)): Either[CursorHistory, Next[A, (List[A], Int)]] =
    moveList(CursorOp.MoveLeft, updIdx(_ - 1))

  /** If the focus is a list, move `n` elements to the left
    *
    * @param n How many elements to move
    * @param ev Evidence that the current focus is a `(List[A], Int)`
    * @see def moveList
    */
  final def leftN(n: Int)(implicit ev: Focus =:= (List[A], Int)): Either[CursorHistory, Next[A, (List[A], Int)]] =
    moveList(CursorOp.LeftN(n), updIdx(_ - n))

  /** If the focus is a list, move one element to the right
    *
    * @param ev Evidence that the current focus is a `(List[A], Int)`
    * @see def moveList
    */
  final def right(implicit ev: Focus =:= (List[A], Int)): Either[CursorHistory, Next[A, (List[A], Int)]] =
    moveList(CursorOp.MoveRight, updIdx(_ + 1))

  /** If the focus is a list, move `n` elements to the right
    *
    * @param n How many elements to move
    * @param ev Evidence that the current focus is a `(List[A], Int)`
    * @see def moveList
    */
  final def rightN(n: Int)(implicit ev: Focus =:= (List[A], Int)): Either[CursorHistory, Next[A, (List[A], Int)]] =
    moveList(CursorOp.RightN(n), updIdx(_ + n))

  /** If the focus is a list, move to the first element
    *
    * @param ev Evidence that the current focus is a `(List[A], Int)`
    * @see def moveList
    */
  final def first(implicit ev: Focus =:= (List[A], Int)): Either[CursorHistory, Next[A, (List[A], Int)]] =
    moveList(CursorOp.MoveFirst, updIdx(_ => 0))

  /** If the current value is a `List[B]`, move to the first element in the list
    *
    * @tparam B The type of values in the list
    * @param ev Evidence that the current focus is a `List[B]`
    * @return A new cursor focused on the first element in the list, `None` if the list was empty
    */
  final def downArray[B](implicit ev: A =:= List[B]): Either[CursorHistory, Next[B, (List[B], Int)]] =
    move(CursorOp.DownArray(false))(ev(value) |> (l =>
      Either.fromOption(l.headOption.map((l, 0) -> _), failedHistory(CursorOp.DownArray(true)))))

  /** If the current value is a `NonEmptyList[B]`, move to the first element in the list
    *
    * @tparam B The type of values in the list
    * @param ev Evidence that the current focus is a `List[B]`
    * @return A new cursor focused on the first element in the list, `None` if the list was empty
    */
  final def downNel[B](implicit ev: A =:= NonEmptyList[B]): Next[B, (List[B], Int)] =
    move(CursorOp.DownArray(false))(ev(value) |> (n => id(((n.toList, 0), n.head))))

  /** If the current value has a field at a key of type `K`, move to the value at the key
    *
    * @tparam K The type of the key
    * @param k The key itself
    * @return A new cursor focused on the value of the field
    */
  final def downField[K <: String](k: K)(implicit l: KLens[A, K]): Next[l.Out, l.Out] =
    move(CursorOp.DownField(k))(dup(l(value)))

  /** Alias for `downField` */
  final def get[K <: Singleton with String](k: K)(implicit l: KLens[A, K]): Next[l.Out, l.Out] =
    downField[K](k)

  /** Alias for `downField`, provides shapeless record-like access, e.g. `x("foo")` */
  final def apply[K <: Singleton with String](k: K)(implicit l: KLens[A, K]): Next[l.Out, l.Out] =
    downField[K](k)

  /** Alias for `downField` that allows dynamic access, e.g. `x.foo` instead of `x.downField("foo")`
    *
    * @see https://www.scala-lang.org/api/current/scala/Dynamic.html
    */
  final def selectDynamic(k: String)(implicit l: KLens[A, tag.@@[String, k.type]]): Next[l.Out, l.Out] =
    downField(tag[k.type](k))

  /** Return a successful validation of the current value */
  final def valid[L]: ValidatedNel[L, A] =
    value.validNel[L]

  /** Validate the current value
    *
    * @tparam B The success type of the validation
    * @param f A validation given the current value of type `A`
    * @return The result of running the current value through the validation
    */
  final def validate[L, B](f: A => ValidatedNel[L, B]): ValidatedNel[L, B] =
    f(value)

  /** Validate the current value, lifting the failure case into a `NonEmptyList` */
  final def validate[L, B](f: A => Validated[L, B])(implicit @deprecated("unused", "") d: Dummy1): ValidatedNel[L, B] =
    validate(f(_: A).leftMap(NonEmptyList.of(_)))

  /** Validate the current optional cursor given a validation
    * for a non-optional cursor focused on a value of type `B`
    *
    * @tparam B The inner type of the `Option` that `A` is proven to be
    * @tparam C The success type of the validation
    * @param f A validation given a cursor focused on a value of type `B` (if the current value was a `Some[A]`)
    * @return The result of running the current `Some[A]` value through the validation
    */
  final def validateO[L, B, C](f: Next[B, B] => ValidatedNel[L, C])(implicit ev: A =:= Option[B]): ValidatedNel[L, Option[C]] =
    validate(ev(_: A).fold(Option.empty[C].validNel[L])(b => f(map(_ => b)).map(Some(_))))

  /** Validate the current optional cursor, lifting the failure case into a `NonEmptyList` */
  final def validateO[L, B, C](f: Next[B, B] => Validated[L, C])(implicit ev: A =:= Option[B], @deprecated("unused", "") d: Dummy1): ValidatedNel[L, Option[C]] =
    validateO(f(_: Next[B, B]).leftMap(NonEmptyList.of(_)))

  /** Validate the current optional value */
  final def validateO[L, B, C](f: B => ValidatedNel[L, C])(implicit ev: A =:= Option[B], @deprecated("unused", "") d: Dummy2): ValidatedNel[L, Option[C]] =
    validateO((c: Next[B, B]) => f(c.value))

  /** Validate the current optional value, lifting the failure case into a `NonEmptyList` */
  final def validateO[L, B, C](f: B => Validated[L, C])(implicit ev: A =:= Option[B], @deprecated("unused", "") d: Dummy3): ValidatedNel[L, Option[C]] =
    validateO(f(_: B).leftMap(NonEmptyList.of(_)))

  /** Ensure the current value matches a boolean predicate
    *
    * @param e The validation errors to return if the predicate returns false
    * @param f A predicate function from A to boolean
    * @return A validation of the value, successful if the predicate was true
    */
  final def ensure[L](e: => NonEmptyList[L])(f: A => Boolean): ValidatedNel[L, A] =
    validate((a: A) => Validated.cond(f(a), a, e))

  /** Ensure the current value matches a boolean predicate and lift the failure case into a `NonEmptyList` */
  final def ensure[L](e: => L)(f: A => Boolean)(implicit @deprecated("unused", "") d: Dummy1): ValidatedNel[L, A] =
    ensure(NonEmptyList.of(e))(f)

  /** If the current value is a `NonEmptyList[B]`, traverse it with the provided validation
    *
    * @tparam B The type of values in the list
    * @tparam C The success type of the validation
    * @param f A function to validate a single element in the list
    * @param err A function to produce a failure case when the list is empty
    * @param ev Evidence that the current focus is a `List[B]`
    * @return The result of traversing the list and running the validation on each value
    */
  final def parseNel[L, B, C](f: GCursor.Arr[T, B] => ValidatedNel[L, C])(implicit ev: A =:= NonEmptyList[B]): ValidatedNel[L, NonEmptyList[C]] = {
    @tailrec
    def go(acc: ValidatedNel[L, NonEmptyList[C]], next: Either[CursorHistory, GCursor.Arr[T, B]]): ValidatedNel[L, NonEmptyList[C]] =
      next match {
        case Right(c) => go(acc |+| f(c).map(NonEmptyList.of(_)), c.right)
        case Left(_) => acc
      }

    downNel[B] |> (c => go(f(c).map(NonEmptyList.of(_)), Right(c)))
  }

  private final def parseNel0[L, B, C, O](f: GCursor.Arr[T, B] => ValidatedNel[L, C])(
    succ: ValidatedNel[L, NonEmptyList[C]] => ValidatedNel[L, O],
    err: CursorHistory => ValidatedNel[L, O]
  )(implicit ev: A =:= List[B]): ValidatedNel[L, O] =
    ev(value).toNel.fold(err(failedHistory(CursorOp.DownArray(true))))(n => succ(map(_ => n).parseNel(f)))

  /** If the current value is a `List[B]`, prove that it's non-empty and traverse it with the provided validation
    *
    * @tparam B The type of values in the list
    * @tparam C The success type of the validation
    * @param f A function to validate a single element in the list
    * @param err A function to produce a failure case when the list is empty
    * @param ev Evidence that the current focus is a `List[B]`
    * @return The result of traversing the list and running the validation on each value
    */
  final def parseNel[L, B, C](
    f: GCursor.Arr[T, B] => ValidatedNel[L, C],
    err: CursorHistory => NonEmptyList[L]
  )(implicit ev: A =:= List[B]): ValidatedNel[L, NonEmptyList[C]] =
    parseNel0(f)(identity, err(_).invalid[NonEmptyList[C]])

  /** If the current value is a `List[B]`, traverse it with the provided validation
    *
    * @tparam B The type of values in the list
    * @tparam C The success type of the validation
    * @param f A function to validate a single element in the list
    * @param ev Evidence that the current focus is a `List[B]`
    * @return The result of traversing the list and running the validation on each value
    */
  final def parseList[L, B, C](f: GCursor.Arr[T, B] => ValidatedNel[L, C])(implicit ev: A =:= List[B]): ValidatedNel[L, List[C]] =
    parseNel0(f)(_.map(_.toList), _ => List[C]().validNel[L])
}

object GCursor {
  type Aux[T, A, F] = GCursor[T, A] {
    type Focus = F
  }

  type AuxL[T, A, F, L <: GCursor[T, _]] = GCursor[T, A] {
    type Focus = F
    type Last = L
  }

  type Top[T] = AuxL[T, T, T, Nothing]

  type Arr[T, A] = Aux[T, A, (List[A], Int)]

  /** Construct a `GCursor` for a given value of type `T` focused on the same value
    *
    * @tparam T The root type of the cursor
    * @param t The root value to focus on
    * @return The constructed cursor
    */
  def top[T](t: T): Top[T] =
    new GCursor[T, T](t, CursorOp.MoveTop) { self =>
      type Focus = T
      type Last = Nothing
      lazy val root = self
      lazy val focus = t
      lazy val last: Option[Nothing] = None
    }

  private[GCursor] implicit class PipeOps[A](private val a: A) extends AnyVal { def |>[B](f: A => B): B = f(a) }

  private[GCursor] sealed trait Dummy1; object Dummy1 { implicit val inst: Dummy1 = new Dummy1 {} }
  private[GCursor] sealed trait Dummy2; object Dummy2 { implicit val inst: Dummy2 = new Dummy2 {} }
  private[GCursor] sealed trait Dummy3; object Dummy3 { implicit val inst: Dummy3 = new Dummy3 {} }
}
