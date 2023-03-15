package typify
package labelled

import scala.language.experimental.macros
import scala.language.implicitConversions

private[typify] trait LabelledPackageAux {
  @inline final implicit def singletonToSingletonOps(t: Any): shapeless.syntax.SingletonOps =
    macro shapeless.SingletonTypeMacros.mkSingletonOps

  @inline final implicit def labelledToLabelledOps[K, V](l: K ->> V): LabelledOps[K, V] =
    new LabelledOps[K, V](l)
}

final class LabelledOps[K, V](private val labelled: K ->> V) extends AnyVal {
  @inline final def label(implicit k: ValueOf[K]): K = k.value
}
