package typify

import typify.parsedany._
import org.scalacheck.Properties
import scala.util.chaining._

object MakeAny extends MakeParsed[Any] {
  import implicits._

  def make[A](k: String, v: A)(implicit mp: MustParse[A]): Cursor[Any] =
    Cursor.top(Map(k -> v): Any)

  def to[A](v: A)(implicit mp: MustParse[A]): Cursor[Any] =
    Cursor.top(v: Any)
}

object AnyCanParse extends Properties("CanParse.Any") {
  property.update("parses required types correctly", new CanParseProp(MakeAny: MakeParsed[Any]).recursive).pipe(_ => ())
}

