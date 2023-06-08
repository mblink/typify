package typify.tagged

import scala.language.implicitConversions

private[typify] trait OpaquePackageAux {
  final type OpaqueTagged[+A, T] = OpaqueTagged.Type[A, T]

  @inline final def opaqueTag[T]: OpaqueTagged.Of[T] = new OpaqueTagged.Of[T]
  @inline final def subst[F[_], A, T](a: F[A]): F[OpaqueTagged[A, T]] = opaqueTag[T].subst(a)
  @inline final def untag[A, T](a: OpaqueTagged[A, T]): A = opaqueTag[T].untag(a)
  @inline final def unsubst[F[_], A, T](a: F[OpaqueTagged[A, T]]): F[A] = opaqueTag[T].unsubst(a)
}

object OpaqueTagged {
  type Type[+A, T] <: Tagged with Scope[A, T]
  private[tagged] type Tagged = Any { type Tag }
  private[tagged] trait Scope[+A, T] extends Any
  object Scope {
    @inline implicit def taggedToOps[A, T](tagged: OpaqueTagged[A, T]): Ops[A, T] =
      new Ops[A, T](tagged)
  }

  final class Of[T](private val dummy: Boolean = false) extends AnyVal {
    @inline final def apply[@specialized A](a: A): OpaqueTagged[A, T] = a.asInstanceOf[OpaqueTagged[A, T]]
    @inline final def subst[F[_], @specialized A](fa: F[A]): F[OpaqueTagged[A, T]] = fa.asInstanceOf[F[OpaqueTagged[A, T]]]
    @inline final def unsubst[@specialized A, F[_]](fa: F[OpaqueTagged[A, T]]): F[A] = fa.asInstanceOf[F[A]]
    @inline final def untag[@specialized A](a: OpaqueTagged[A, T]): A = unsubst[A, Lambda[a => a]](a)
    @inline final def tag(implicit t: ValueOf[T]): T = t.value
  }

  @inline def apply[T]: Of[T] = opaqueTag[T]

  final class Ops[A, T](private val tagged: OpaqueTagged[A, T]) extends AnyVal {
    @inline private final def t: Of[T] = tag[T]
    final def map[B](f: A => B): OpaqueTagged[B, T] = t.apply(f(t.untag(tagged)))
    final def untag: A = t.untag(tagged)
  }

  @inline def deriving[TC[_], A, T](implicit tc: TC[A]): TC[OpaqueTagged[A, T]] =
    tc.asInstanceOf[TC[OpaqueTagged[A, T]]]

  @inline def derivingK[TC[_[_]], T](implicit tc: TC[Lambda[a => a]]): TC[OpaqueTagged[*, T]] =
    tc.asInstanceOf[TC[OpaqueTagged[*, T]]]
}
