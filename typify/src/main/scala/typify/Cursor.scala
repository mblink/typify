package typify

import cats.Applicative
import cats.instances.option._
import cats.instances.vector._
import cats.kernel.Eq
import cats.syntax.eq._
import java.io.Serializable
import scala.annotation.tailrec
import scala.collection.immutable.ListMap

/**
 * A zipper that represents a position in a value of type `A` and supports navigation and modification.
 *
 * The `focus` represents the current position of the cursor; it may be updated with `withFocus` or
 * changed using navigation methods like `left` and `right`.
 */
sealed abstract class Cursor[A](private val lastCursor: Option[Cursor[A]], private val lastOp: Option[CursorOp]) extends Serializable {

  def show: String

  /**
   * The current location in the document.
   */
  def focus: Option[A]

  def replace(newValue: A, cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A]
  def addOp(cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A]

  /**
   * The operations that have been performed so far.
   */
  final def history: Vector[CursorOp] = {
    @tailrec def go(res: Vector[CursorOp], next: Option[Cursor[A]]): Vector[CursorOp] = next match {
      case Some(c) => go(c.lastOp.fold(res)(res :+ _), c.lastCursor)
      case None => res
    }

    go(Vector(), Some(this))
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
   * Return the cursor as an [[Cursor[A]]] if it was successful.
   */
  def success: Option[Cursor[A]]

  /**
   * Return to the root of the document.
   */
  def top: Option[A]

  /**
   * Modify the focus using the given function.
   */
  def withFocus(f: A => A): Cursor[A]

  /**
   * Modify the focus in a context using the given function.
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
   * If the focus is an element in JSON array, move to the left the given number of times.
   *
   * A negative value will move the cursor right.
   */
  def leftN(n: Int): Cursor[A]

  /**
   * If the focus is an element in JSON array, move to the right the given number of times.
   *
   * A negative value will move the cursor left.
   */
  def rightN(n: Int): Cursor[A]

  /**
   * If the focus is a JSON array, move to its first element.
   */
  def downArray: Cursor[A]

  /**
   * If the focus is a JSON array, move to the element at the given index.
   */
  def downN(n: Int): Cursor[A]

  /**
   * If the focus is a value in a JSON object, move to a sibling with the given key.
   */
  def field(k: String): Cursor[A]

  /**
   * If the focus is a JSON object, move to the value of the given key.
   */
  def downField(k: String): Cursor[A]

  /**
   * Replay an operation against this cursor.
   */
  final def replayOne(op: CursorOp): Cursor[A] = op match {
    case CursorOp.MoveLeft       => left
    case CursorOp.MoveRight      => right
    case CursorOp.MoveFirst      => first
    case CursorOp.MoveUp         => up
    case CursorOp.LeftN(n)       => leftN(n)
    case CursorOp.RightN(n)      => rightN(n)
    case CursorOp.Field(k)       => field(k)
    case CursorOp.DownField(k)   => downField(k)
    case CursorOp.DownArray      => downArray
    case CursorOp.DownN(n)       => downN(n)
    case CursorOp.DeleteGoParent => delete
  }

  /**
   * Replay history (a list of operations in reverse "chronological" order) against this cursor.
   */
  final def replay(history: List[CursorOp]): Cursor[A] = history.foldRight(this)((op, c) => c.replayOne(op))

  private[typify] final def fail(op: CursorOp): Cursor[A] = Cursor.Failed[A](Some(this), Some(op))
}

final object Cursor {
  def top[A: Generic](value: A): Cursor[A] = new Top[A](value)(None, None)

  def at[A: Generic](value: A, root: Vector[String]): Cursor[A] =
    root.foldLeft(top(value))(_.downField(_))

  implicit def eqCursor[A: Eq]: Eq[Cursor[A]] =
    Eq.instance((a, b) => a.focus === b.focus && a.history === b.history)

  sealed trait WithValue[A] { self: Cursor[A] =>
    def value: A
    implicit def gen: Generic[A]

    final def withFocus(f: A => A): Cursor[A] =
      replace(f(value), Some(self), None)

    final def withFocusM[F[_]](f: A => F[A])(implicit F: Applicative[F]): F[Cursor[A]] =
      F.map(f(value))(replace(_, Some(self), None))

    final def succeeded: Boolean = true
    final def success: Option[Cursor[A]] = Some(this)

    final def focus: Option[A] = Some(value)

    final def values: Option[Iterable[A]] = gen.toValues(value)

    final def keys: Option[Iterable[String]] = gen.toFields(value).map(_.map(_._1))

    final def top: Option[A] = {
      @tailrec def go(curr: Cursor[A]): Option[A] = curr match {
        case Top(v) => Some(v)
        case _ => go(curr.up)
      }

      go(this)
    }

    private def withOp[O](op: CursorOp)(f: (CursorOp, Cursor[A]) => O): O = f(op, fail(op))

    final def downArray: Cursor[A] = withOp(CursorOp.DownArray) { case (o, f) =>
      gen.toValues(value).fold(f)(
        Some(_).filter(_.nonEmpty).fold(f)(new Array(_, 0, this, false)(Some(this), Some(o))))
    }

    final def downField(k: String): Cursor[A] = withOp(CursorOp.DownField(k)) { case (o, f) =>
      gen.toFields(value).fold(f)(fs =>
        fs.toMap.get(k).fold(f)(_ => new Object(fs, k, this, false)(Some(this), Some(o))))
    }

    final def downN(n: Int): Cursor[A] = withOp(CursorOp.DownN(n)) { case (o, f) =>
      gen.toValues(value).fold(f)(
        Some(_).filter(x => n >= 0 && x.size > n).fold(f)(new Array(_, n, this, false)(Some(this), Some(o))))
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
    lastOp: Option[CursorOp]
  )(implicit val gen: Generic[A]) extends Cursor[A](lastCursor, lastOp) with WithValue[A] {
    override def show: String = s"Array($values, $index, $parent, $changed)($lastCursor, $lastOp)"

    def value: A = values(index)

    private[this] def valuesExcept: Vector[A] = values.take(index) ++ values.drop(index + 1)

    def replace(newValue: A, cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A] =
      new Array(values.updated(index, newValue), index, parent, true)(cursor, op)

    def addOp(cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A] =
      new Array(values, index, parent, changed)(cursor, op)

    def up: Cursor[A] =
      if (!changed) parent.addOp(Some(this), Some(CursorOp.MoveUp))
      else parent.replace(gen.fromValues(values), Some(this), Some(CursorOp.MoveUp))

    def delete: Cursor[A] = parent.replace(gen.fromValues(valuesExcept), Some(this), Some(CursorOp.DeleteGoParent))

    def hasLeft: Boolean = index != 0

    def left: Cursor[A] =
      if (hasLeft) new Array(values, index - 1, parent, changed)(Some(this), Some(CursorOp.MoveLeft))
      else fail(CursorOp.MoveLeft)

    def hasRight: Boolean = index != values.size - 1

    def right: Cursor[A] =
      if (hasRight) new Array(values, index + 1, parent, changed)(Some(this), Some(CursorOp.MoveRight))
      else fail(CursorOp.MoveRight)

    def first: Cursor[A] = new Array(values, 0, parent, changed)(Some(this), Some(CursorOp.MoveFirst))
    def field(k: String): Cursor[A] = fail(CursorOp.Field(k))
  }

  final class Object[A](fields: ListMap[String, A], key: String, parent: Cursor[A], changed: Boolean)(
    lastCursor: Option[Cursor[A]],
    lastOp: Option[CursorOp]
  )(implicit val gen: Generic[A]) extends Cursor[A](lastCursor, lastOp) with WithValue[A] {
    override def show: String = s"Object($fields, $key, $parent, $changed)($lastCursor, $lastOp)"

    def value: A = fields(key)

    def replace(newValue: A, cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A] =
      new Object[A](fields + (key -> newValue), key, parent, true)(cursor, op)

    def addOp(cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A] =
      new Object[A](fields, key, parent, changed)(cursor, op)

    def up: Cursor[A] =
      if (!changed) parent.addOp(Some(this), Some(CursorOp.MoveUp))
      else parent.replace(gen.fromFields(fields), Some(this), Some(CursorOp.MoveUp))

    def delete: Cursor[A] =
      parent.replace(gen.fromFields(fields - key), Some(this), Some(CursorOp.DeleteGoParent))

    def field(k: String): Cursor[A] =
      if (!fields.contains(k)) fail(CursorOp.Field(k))
      else new Object[A](fields, k, parent, changed)(Some(this), Some(CursorOp.Field(k)))

    def left: Cursor[A] = fail(CursorOp.MoveLeft)
    def right: Cursor[A] = fail(CursorOp.MoveRight)
    def first: Cursor[A] = fail(CursorOp.MoveFirst)
  }

  final case class Failed[A](
    lastCursor: Option[Cursor[A]],
    lastOp: Option[CursorOp]
  ) extends Cursor[A](lastCursor, lastOp) {
    override def show: String = s"Failed($lastCursor, $lastOp)"

    def succeeded: Boolean = false
    def success: Option[Cursor[A]] = None

    def replace(newValue: A, cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A] = this
    def addOp(cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A] = this

    def focus: Option[A] = None
    def top: Option[A] = None

    def withFocus(f: A => A): Cursor[A] = this
    def withFocusM[F[_]](f: A => F[A])(implicit F: Applicative[F]): F[Cursor[A]] = F.pure(this)

    def values: Option[Iterable[A]] = None
    def keys: Option[Iterable[String]] = None

    def downArray: Cursor[A] = this
    def downField(k: String): Cursor[A] = this
    def downN(n: Int): Cursor[A] = this
    def leftN(n: Int): Cursor[A] = this
    def rightN(n: Int): Cursor[A] = this
    def up: Cursor[A] = this

    def left: Cursor[A] = this
    def right: Cursor[A] = this
    def first: Cursor[A] = this
    def last: Cursor[A] = this

    def delete: Cursor[A] = this

    def field(k: String): Cursor[A] = this
  }

  final case class Top[A](value: A)(
    lastCursor: Option[Cursor[A]],
    lastOp: Option[CursorOp]
  )(implicit val gen: Generic[A]) extends Cursor[A](lastCursor, lastOp) with WithValue[A] {
    override def show: String = s"Top($lastCursor, $lastOp)"

    def replace(newValue: A, cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A] =
      new Top(newValue)(cursor, op)

    def addOp(cursor: Option[Cursor[A]], op: Option[CursorOp]): Cursor[A] =
      new Top(value)(cursor, op)

    def up: Cursor[A] = fail(CursorOp.MoveUp)
    def delete: Cursor[A] = fail(CursorOp.DeleteGoParent)

    def left: Cursor[A] = fail(CursorOp.MoveLeft)
    def right: Cursor[A] = fail(CursorOp.MoveRight)
    def first: Cursor[A] = fail(CursorOp.MoveFirst)

    def field(k: String): Cursor[A] = fail(CursorOp.Field(k))
  }
}
