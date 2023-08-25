package typify
package test

import munit.Assertions.assert
import munit.Location
import scala.compiletime.testing.typeChecks

/**
 * A utility which ensures that a code fragment does not typecheck.
 */
object illTyped {
  inline def apply(code: String)(implicit loc: Location): Unit = assert(!typeChecks(code))
  inline def apply(code: String, expected: String)(implicit loc: Location): Unit = apply(code)
}
