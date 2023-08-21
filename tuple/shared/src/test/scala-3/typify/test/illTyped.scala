package typify
package test

import scala.compiletime.testing.typeChecks

/**
 * A utility which ensures that a code fragment does not typecheck.
 */
object illTyped {
  inline def apply(code: String): Unit = assert(!typeChecks(code))
  inline def apply(code: String, expected: String): Unit = apply(code)
}
