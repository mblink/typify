package typify.tagged

private[typify] trait OpaquePackageAux

opaque type OpaqueTagged[+A, T] = A

inline def opaqueTag[T]: OpaqueTagged.Of[T] = OpaqueTagged[T]

object OpaqueTagged {
  final class Of[T](private val dummy: Boolean = false) extends AnyVal {
    inline final def apply[@specialized A](a: A): OpaqueTagged[A, T] = a
    inline final def subst[F[_], @specialized A](fa: F[A]): F[OpaqueTagged[A, T]] = fa
    inline final def unsubst[@specialized A, F[_]](fa: F[OpaqueTagged[A, T]]): F[A] = fa
    inline final def untag[@specialized A](a: OpaqueTagged[A, T]): A = unsubst[A, [a] =>> a](a)
    inline final def tag(implicit t: ValueOf[T]): T = t.value
  }

  inline def apply[T]: Of[T] = new Of[T]

  extension [T, A](x: OpaqueTagged[A, T]) {
    inline final def map[B](f: A => B): OpaqueTagged[B, T] = f(x)
    inline final def untag: A = x
    inline final def tag(implicit t: ValueOf[T]): T = t.value
  }

  inline implicit def valueOfOpaqueTagged[A, T](implicit a: ValueOf[A]): ValueOf[OpaqueTagged[A, T]] =
    new ValueOf[OpaqueTagged[A, T]](a.value)

  inline def deriving[TC[_], A, T](using inline tc: TC[A]): TC[OpaqueTagged[A, T]] =
    tc.asInstanceOf[TC[OpaqueTagged[A, T]]]

  inline def derivingK[TC[_[_]], T](using inline tc: TC[[a] =>> a]): TC[OpaqueTagged[*, T]] =
    tc.asInstanceOf[TC[OpaqueTagged[*, T]]]
}
