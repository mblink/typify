package typify

import typify.parsedany._
import org.scalacheck.{Gen, Properties}

object MakeAny extends MakeParsed[Any] {

  import implicits._

  def make[A](k: String, v: A)(implicit mp: MustParse[A]): Any =
    (Map(k -> v): Any)

  def to[A](v: A)(implicit mp: MustParse[A]): Any = (v: Any)
}

object AnyCanParse extends Properties("Any CanParse") {

  val prop = new CanParseProp(MakeAny: MakeParsed[Any])

  property("parses required types correctly") = prop.recursive
}

