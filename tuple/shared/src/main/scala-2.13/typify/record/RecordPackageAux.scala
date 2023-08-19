package typify
package record

import scala.language.implicitConversions
import typify.tuple.Tuple

final class TypifySingletonOps[K <: Singleton](private val k: K) extends AnyVal {
  @inline final def ->>[V](v: V): K ->> V = label[K](v)
}

final class TypifyLabelledOps[K, V](private val kv: K ->> V) extends AnyVal {
  @inline final def label(implicit k: ValueOf[K]): K = k.value
}

private[typify] trait RecordPackageAux {
  final type ->>[K, +V] = tagged.TranslucentTagged[V, K]

  final class LabelPartialAp[K] {
    @inline final def apply[V](v: V): K ->> V = shapeless.labelled.field[K].apply[V](v)
  }

  @inline final def label[K]: LabelPartialAp[K] = new LabelPartialAp[K]

  @inline final implicit def toTypifySingletonOps[K <: Singleton](k: K): TypifySingletonOps[K] = new TypifySingletonOps[K](k)
  @inline final implicit def toTypifyLabelledOps[K, V](kv: K ->> V): TypifyLabelledOps[K, V] = new TypifyLabelledOps[K, V](kv)

  @inline final implicit def tupleToRecordOps[T <: Tuple](t: T): shapeless.syntax.RecordOps[T] =
    new shapeless.syntax.RecordOps[T](t)

  final type LacksKey[T <: Tuple, K] = shapeless.ops.record.LacksKey[T, K]
  final val LacksKey: shapeless.ops.record.LacksKey.type = shapeless.ops.record.LacksKey

  final type Keys[T <: Tuple] = shapeless.ops.record.Keys[T]
  final val Keys: shapeless.ops.record.Keys.type = shapeless.ops.record.Keys

  final type Merger[L <: Tuple, M <: Tuple] = shapeless.ops.record.Merger[L, M]
  final val Merger: shapeless.ops.record.Merger.type = shapeless.ops.record.Merger

  final type Modifier[T <: Tuple, K, A, B] = shapeless.ops.record.Modifier[T, K, A, B]
  final val Modifier: shapeless.ops.record.Modifier.type = shapeless.ops.record.Modifier

  final type Remover[T <: Tuple, K] = shapeless.ops.record.Remover[T, K]
  final val Remover: shapeless.ops.record.Remover.type = shapeless.ops.record.Remover

  final type Renamer[T <: Tuple, K1, K2] = shapeless.ops.record.Renamer[T, K1, K2]
  final val Renamer: shapeless.ops.record.Renamer.type = shapeless.ops.record.Renamer

  final type Selector[T <: Tuple, K] = shapeless.ops.record.Selector[T, K]
  final val Selector: shapeless.ops.record.Selector.type = shapeless.ops.record.Selector

  final type SelectAll[L <: Tuple, K <: Tuple] = shapeless.ops.record.SelectAll[L, K]
  final val SelectAll: shapeless.ops.record.SelectAll.type = shapeless.ops.record.SelectAll

  final type Updater[T <: Tuple, F] = shapeless.ops.record.Updater[T, F]
  final val Updater: shapeless.ops.record.Updater.type = shapeless.ops.record.Updater

  final type Values[L <: Tuple] = shapeless.ops.record.Values[L]
  final val Values: shapeless.ops.record.Values.type = shapeless.ops.record.Values
}
