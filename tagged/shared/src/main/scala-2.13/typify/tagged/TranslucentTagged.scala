package typify.tagged

private[typify] trait TranslucentPackageAux {
  final type TranslucentTagged[+A, T] = shapeless.labelled.FieldType[T, A]

  @inline def translucentTag[T]: TranslucentTagged.Of[T] = TranslucentTagged[T]
}

object TranslucentTagged {
  final class Of[T](private val dummy: Boolean = false) extends AnyVal {
    @inline final def apply[A](a: A): TranslucentTagged[A, T] = a.asInstanceOf[TranslucentTagged[A, T]]
  }

  @inline def apply[T]: Of[T] = new Of[T]

  final class Ops[A, T](private val tagged: TranslucentTagged[A, T]) extends AnyVal {
    @inline final def tag(implicit t: ValueOf[T]): T = t.value
    @inline final def label(implicit t: ValueOf[T]): T = t.value
  }
}
