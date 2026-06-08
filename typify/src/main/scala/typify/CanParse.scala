package typify

import cats.data.ValidatedNel

trait CanParse[T, P] {
  def apply(cursor: Cursor[P]): ValidatedNel[ParseError[P], T]
  def parse(key: String, cursor: Cursor[P]): ValidatedNel[ParseError[P], T] = apply(cursor.downField(key))
}
