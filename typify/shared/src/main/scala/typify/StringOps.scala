package typify

import cats.syntax.either._
import scala.language.implicitConversions

object StringOps {
  implicit class StringOpsImpl(val s: String) extends AnyVal {
    def parseInt: Option[Int] = Either.catchNonFatal(s.toInt).toOption
    def parseLong: Option[Long] = Either.catchNonFatal(s.toLong).toOption
    def parseDouble: Option[Double] = Either.catchNonFatal(s.toDouble).toOption
    def parseBoolean: Option[Boolean] = Either.catchNonFatal(s.toBoolean).toOption
  }
}

trait StringOps {
  @inline implicit def toStringOpsImpl(s: String): StringOps.StringOpsImpl = new StringOps.StringOpsImpl(s)
}
