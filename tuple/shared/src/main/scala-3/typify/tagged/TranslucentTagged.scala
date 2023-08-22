package typify.tagged

private[typify] trait TranslucentPackageAux

type TranslucentTagged[+A, T] = A with TranslucentTag[A, T]
type TranslucentTag[+A, T]

inline def translucentTag[T]: TranslucentTagged.Of[T] = TranslucentTagged[T]

object TranslucentTagged {
  final class Of[T](private val dummy: Boolean = false) extends AnyVal {
    inline final def apply[A](a: A): TranslucentTagged[A, T] = a.asInstanceOf[TranslucentTagged[A, T]]
  }

  inline def apply[T]: Of[T] = new Of[T]

  extension [A, T](x: TranslucentTagged[A, T]) {
    inline final def tag(using t: ValueOf[T]): T = t.value
    inline final def label(using t: ValueOf[T]): T = t.value
  }
}
