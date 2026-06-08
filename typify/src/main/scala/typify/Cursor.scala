package typify

import cats.{Applicative, Eq}
import cats.syntax.eq._
import java.io.Serializable
import scala.annotation.tailrec
import scala.collection.immutable.ListMap

/**
 * A zipper that represents a position in a value of type `A` and supports navigation and modification.
 *
 * The `focus` represents the current position of the cursor; it may be updated with `withFocus` or
 * changed implicit navigation methods like `left` and `right`.
 */
sealed abstract class Cursor[A](
  private val lastCursor: Option[Cursor[A]],
  private val lastOp: Either[CursorOp, CursorOp]
) extends Serializable {

  def show: String

  /**
   * The current location in the document.
   */
  def focus: Option[A]

  def replace(newValue: A, cursor: Option[Cursor[A]], op: CursorOp): Cursor[A]
  def addOp(cursor: Option[Cursor[A]], op: CursorOp): Cursor[A]

  /**
   * The operations that have been performed so far.
   */
  final def history: CursorHistory[A] = {
    @tailrec def go(res: CursorHistory[A], next: Option[Cursor[A]]): CursorHistory[A] = next match {
      case Some(c) => go(res :+ c.lastOp, c.lastCursor)
      case None => res
    }

    go(CursorHistory.empty[A], Some(this))
  }

  /**
   * Indicate whether this cursor represents the result of a successful
   * operation.
   */
  def succeeded: Boolean

  /**
   * Indicate whether this cursor represents the result of an unsuccessful
   * operation.
   */
  final def failed: Boolean = !succeeded

  /**
   * Return the cursor as a [[Cursor]] if it was successful.
   */
  def success: Option[Cursor[A]]

  /**
   * Modify the focus implicit the implicit val function.
   */
  def withFocus(f: A => A): Cursor[A]

  /**
   * Modify the focus in a context implicit the implicit def function.
   */
  def withFocusM[F[_]](f: A => F[A])(implicit F: Applicative[F]): F[Cursor[A]]

  /**
   * Replace the focus.
   */
  final def set(j: A): Cursor[A] = withFocus(_ => j)

  /**
   * If the focus is a JSON array, return its elements.
   */
  def values: Option[Iterable[A]]

  /**
   * If the focus is a JSON object, return its field names in their original order.
   */
  def keys: Option[Iterable[String]]

  /**
   * Delete the focus and move to its parent.
   */
  def delete: Cursor[A]

  /**
   * Return to the root of the document.
   */
  def top: Cursor[A]

  /**
   * Move the focus to the parent.
   */
  def up: Cursor[A]

  /**
   * If the focus is an element in a JSON array, move to the left.
   */
  def left: Cursor[A]

  /**
   * If the focus is an element in a JSON array, move to the right.
   */
  def right: Cursor[A]

  /**
   * If the focus is an element in a JSON array, move to the first element.
   */
  def first: Cursor[A]

  /**
   * If the focus is an element in JSON array, move to the left the implicit val number of times.
   *
   * A negative value will move the cursor right.
   */
  def leftN(n: Int): Cursor[A]

  /**
   * If the focus is an element in JSON array, move to the right the implicit val number of times.
   *
   * A negative value will move the cursor left.
   */
  def rightN(n: Int): Cursor[A]

  /**
   * If the focus is a JSON array, move to its first element.
   */
  def downArray: Cursor[A]

  /**
   * If the focus is a JSON array, move to the element at the implicit val index.
   */
  def downN(n: Int): Cursor[A]

  /**
   * If the focus is a value in a JSON object, move to a sibling with the implicit val key.
   */
  def field(k: String): Cursor[A]

  /**
   * If the focus is a JSON object, move to the value of the implicit val key.
   */
  def downField(k: String): Cursor[A]

  private[typify] final def fail(op: CursorOp): Cursor[A] = Cursor.Failed[A](Some(this), op)
}

object Cursor {
  def top[A: Generic](value: A): Cursor[A] = new Top[A](value)(None, None)

  def at[A: Generic](value: A, root: Vector[String]): Cursor[A] =
    root.foldLeft(top(value))(_.downField(_))

  implicit def eqCursor[A: Eq]: Eq[Cursor[A]] =
    Eq.instance((a, b) => a.focus === b.focus && a.history === b.history)

  sealed trait WithValue[A] { self: Cursor[A] =>
    def value: A
    implicit val gen: Generic[A]

    final def withFocus(f: A => A): Cursor[A] =
      replace(f(value), Some(self), CursorOp.WithFocus(f))

    final def withFocusM[F[_]](f: A => F[A])(implicit F: Applicative[F]): F[Cursor[A]] =
      F.map(f(value))(a => replace(a, Some(self), CursorOp.WithFocus((_: A) => a)))

    final def succeeded: Boolean = true
    final def success: Option[Cursor[A]] = Some(this)

    final def focus: Option[A] = Some(value)

    final def values: Option[Iterable[A]] = gen.toValues(value)

    final def keys: Option[Iterable[String]] = gen.toFields(value).map(_.map(_._1))

    private def withOp[Op <: CursorOp, O](op: Op)(f: (Op, Cursor[A]) => O): O = f(op, fail(op))

    final def top: Cursor[A] = withOp(CursorOp.MoveTop) { case (o, f) =>
      @tailrec def go(curr: Cursor[A]): Cursor[A] = curr match {
        case t @ Top(_) => t.addOp(Some(this), o)
        case _ @ Failed(_, _) => f
        case _ => go(curr.up)
      }

      go(this)
    }

    final def downArray: Cursor[A] = withOp(CursorOp.DownArray(false)) { case (o, f) =>
      gen.toValues(value).fold(f)(_ match {
        case Vector() => fail(o.copy(empty = true))
        case v => new Array(v, 0, this, false)(Some(this), o)
      })
    }

    final def downField(k: String): Cursor[A] = withOp(CursorOp.DownField(k)) { case (o, f) =>
      gen.toFields(value).fold(f)(fs =>
        fs.get(k).fold(f)(_ => new Object(fs, k, this, false)(Some(this), o)))
    }

    final def downN(n: Int): Cursor[A] = withOp(CursorOp.DownN(n, false)) { case (o, f) =>
      gen.toValues(value).fold(f)(_ match {
        case v if n >= 0 && v.size > n => new Array(v, n, this, false)(Some(this), o)
        case _ => fail(o.copy(outOfRange = true))
      })
    }

    final def leftN(n: Int): Cursor[A] =
      if (n < 0) rightN(-n)
      else {
        @tailrec def go(i: Int, c: Cursor[A]): Cursor[A] = if (i == 0) c else go(i - 1, c.left)
        go(n, this)
      }

    final def rightN(n: Int): Cursor[A] =
      if (n < 0) leftN(-n)
      else {
        @tailrec def go(i: Int, c: Cursor[A]): Cursor[A] = if (i == 0) c else go(i - 1, c.right)
        go(n, this)
      }
  }

  final class Array[A](values: Vector[A], index: Int, parent: Cursor[A], changed: Boolean)(
    lastCursor: Option[Cursor[A]],
    lastOp: CursorOp
  )(implicit val gen: Generic[A]) extends Cursor[A](lastCursor, Right(lastOp)) with WithValue[A] {
    override def show: String = s"Array($values, $index, $parent, $changed)($lastCursor, $lastOp)"

    def value: A = values(index)

    private[this] def valuesExcept: Vector[A] = values.take(index) ++ values.drop(index + 1)

    def replace(newValue: A, cursor: Option[Cursor[A]], op: CursorOp): Cursor[A] =
      new Array(values.updated(index, newValue), index, parent, true)(cursor, op)

    def addOp(cursor: Option[Cursor[A]], op: CursorOp): Cursor[A] =
      new Array(values, index, parent, changed)(cursor, op)

    def up: Cursor[A] =
      if (!changed) parent.addOp(Some(this), CursorOp.MoveUp)
      else parent.replace(gen.fromValues(values), Some(this), CursorOp.MoveUp)

    def delete: Cursor[A] = parent.replace(gen.fromValues(valuesExcept), Some(this), CursorOp.DeleteGoParent)

    def hasLeft: Boolean = index != 0

    def left: Cursor[A] =
      if (hasLeft) new Array(values, index - 1, parent, changed)(Some(this), CursorOp.MoveLeft)
      else fail(CursorOp.MoveLeft)

    def hasRight: Boolean = index != values.size - 1

    def right: Cursor[A] =
      if (hasRight) new Array(values, index + 1, parent, changed)(Some(this), CursorOp.MoveRight)
      else fail(CursorOp.MoveRight)

    def first: Cursor[A] = new Array(values, 0, parent, changed)(Some(this), CursorOp.MoveFirst)
    def field(k: String): Cursor[A] = fail(CursorOp.Field(k))
  }

  final class Object[A](fields: ListMap[String, A], key: String, parent: Cursor[A], changed: Boolean)(
    lastCursor: Option[Cursor[A]],
    lastOp: CursorOp
  )(implicit val gen: Generic[A]) extends Cursor[A](lastCursor, Right(lastOp)) with WithValue[A] {
    override def show: String = s"Object($fields, $key, $parent, $changed)($lastCursor, $lastOp)"

    def value: A = fields(key)

    def replace(newValue: A, cursor: Option[Cursor[A]], op: CursorOp): Cursor[A] =
      new Object[A](fields + (key -> newValue), key, parent, true)(cursor, op)

    def addOp(cursor: Option[Cursor[A]], op: CursorOp): Cursor[A] =
      new Object[A](fields, key, parent, changed)(cursor, op)

    def up: Cursor[A] =
      if (!changed) parent.addOp(Some(this), CursorOp.MoveUp)
      else parent.replace(gen.fromFields(fields), Some(this), CursorOp.MoveUp)

    def delete: Cursor[A] =
      parent.replace(gen.fromFields(fields - key), Some(this), CursorOp.DeleteGoParent)

    def field(k: String): Cursor[A] =
      if (!fields.contains(k)) fail(CursorOp.Field(k))
      else new Object[A](fields, k, parent, changed)(Some(this), CursorOp.Field(k))

    def left: Cursor[A] = fail(CursorOp.MoveLeft)
    def right: Cursor[A] = fail(CursorOp.MoveRight)
    def first: Cursor[A] = fail(CursorOp.MoveFirst)
  }

  final case class Failed[A](
    lastCursor: Option[Cursor[A]],
    lastOp: CursorOp
  ) extends Cursor[A](lastCursor, Left(lastOp)) {
    override def show: String = s"Failed($lastCursor, $lastOp)"

    private def next(op: CursorOp): Cursor[A] = Failed(Some(this), op)

    def succeeded: Boolean = false
    def success: Option[Cursor[A]] = None

    def replace(newValue: A, cursor: Option[Cursor[A]], op: CursorOp): Cursor[A] = next(op)
    def addOp(cursor: Option[Cursor[A]], op: CursorOp): Cursor[A] = next(op)

    def focus: Option[A] = None
    def withFocus(f: A => A): Cursor[A] = next(CursorOp.WithFocus(f))
    def withFocusM[F[_]](f: A => F[A])(implicit F: Applicative[F]): F[Cursor[A]] = F.pure(next(CursorOp.WithFocus(identity[A])))

    def values: Option[Iterable[A]] = None
    def keys: Option[Iterable[String]] = None

    def downArray: Cursor[A] = next(CursorOp.DownArray(false))
    def downField(k: String): Cursor[A] = next(CursorOp.DownField(k))
    def downN(n: Int): Cursor[A] = next(CursorOp.DownN(n, false))
    def leftN(n: Int): Cursor[A] = next(CursorOp.LeftN(n))
    def rightN(n: Int): Cursor[A] = next(CursorOp.RightN(n))
    def top: Cursor[A] = next(CursorOp.MoveTop)
    def up: Cursor[A] = next(CursorOp.MoveUp)

    def left: Cursor[A] = next(CursorOp.MoveLeft)
    def right: Cursor[A] = next(CursorOp.MoveRight)
    def first: Cursor[A] = next(CursorOp.MoveFirst)

    def delete: Cursor[A] = next(CursorOp.DeleteGoParent)

    def field(k: String): Cursor[A] = next(CursorOp.Field(k))
  }

  final case class Top[A](value: A)(
    lastCursor: Option[Cursor[A]],
    lastOp: Option[CursorOp]
  )(implicit val gen: Generic[A]) extends Cursor[A](lastCursor, Right(lastOp.getOrElse(CursorOp.MoveTop))) with WithValue[A] {
    override def show: String = s"Top($lastCursor, $lastOp)"

    def replace(newValue: A, cursor: Option[Cursor[A]], op: CursorOp): Cursor[A] =
      new Top(newValue)(cursor, Some(op))

    def addOp(cursor: Option[Cursor[A]], op: CursorOp): Cursor[A] =
      new Top(value)(cursor, Some(op))

    def up: Cursor[A] = fail(CursorOp.MoveUp)
    def delete: Cursor[A] = fail(CursorOp.DeleteGoParent)

    def left: Cursor[A] = fail(CursorOp.MoveLeft)
    def right: Cursor[A] = fail(CursorOp.MoveRight)
    def first: Cursor[A] = fail(CursorOp.MoveFirst)

    def field(k: String): Cursor[A] = fail(CursorOp.Field(k))
  }
}
