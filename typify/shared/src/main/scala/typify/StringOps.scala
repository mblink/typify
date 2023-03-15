package typify

import scala.util.Try

final class StringOps(private val s: String) extends AnyVal {
  def parseInt: Option[Int] = Try(s.toInt).toOption
  def parseLong: Option[Long] = Try(s.toLong).toOption
  def parseDouble: Option[Double] = Try(s.toDouble).toOption
  def parseBoolean: Option[Boolean] = Try(s.toBoolean).toOption
}
