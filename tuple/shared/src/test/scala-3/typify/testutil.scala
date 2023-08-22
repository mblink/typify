package typify

import munit.Assertions.assertEquals
import munit.Location

object testutil {
  /**
    * WARNING: type parameter 'A' is inferred as least upper bound of 'expected' and 'actual'.
    * The following snippet will not cause a compilation problem: assertTypedEquals(Record(foo = 1), Record(bar = 1))
    */
  def assertTypedEquals[A](expected: A, actual: A)(using loc: Location): Unit = assertEquals(expected, actual)

  def assertTypedEquals[A](expected: A)(using loc: Location): A => Unit = assertEquals(expected, _)
}
