package typify

import scalaz.{Applicative, Apply, Bifunctor, Bitraverse, EitherT, Equal, Failure, Foldable, Functor, Monad, Semigroup, Show, Success, Traverse, Validation}
import scalaz.syntax.contravariant._

private [typify] final case class ValidationT[F[_], E, A](run: F[Validation[E, A]]) { self =>
  def map[B](f: A => B)(implicit F: Functor[F]): ValidationT[F, E, B] =
    ValidationT(F.map(run)(_.map(f)))

  def flatMap[EE >: E, B](f: A => ValidationT[F, EE, B])(implicit F: Monad[F]): ValidationT[F, EE, B] =
    ValidationT[F, EE, B](F.bind(self.run)(_ match {
      case Success(a) => f(a).run
      case Failure(e) => F.point(Failure(e))
    }))

  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): ValidationT[F, E, B] =
    ValidationT(F.bind(run)(_.traverse(f)))

  def bimap[C, D](f: E => C, g: A => D)(implicit F: Functor[F]): ValidationT[F, C, D] =
    ValidationT(F.map(run)(_.bimap(f, g)))

  def leftMap[D](f: E => D)(implicit F: Functor[F]): ValidationT[F, D, A] =
    bimap(f, identity)

  def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G], F: Traverse[F]): G[ValidationT[F, E, B]] =
    G.map(F.traverse(run)(_.traverse(f)))(ValidationT(_))

  def bitraverse[G[_], C, D](f: E => G[C], g: A => G[D])(implicit G: Applicative[G], F: Traverse[F]) =
    G.map(F.traverse(run)(_.bitraverse(f, g)))(ValidationT(_))

  def foldRight[B](z: => B)(f: (A, => B) => B)(implicit F: Foldable[F]): B =
    F.foldRight(run, z)(_.foldRight(_)(f))

  def ap[B, EE >: E](f: => ValidationT[F, EE, A => B])(implicit F: Apply[F], E: Semigroup[EE]): ValidationT[F, EE, B] =
    ValidationT(F.lift2[Validation[E, A], Validation[EE, A => B], Validation[EE, B]](_.ap(_))(run, f.run))
}

private trait ValidationTFunctions {
  def validationT[F[_], E, A](m: F[Validation[E, A]]): ValidationT[F, E, A] = ValidationT(m)

  def successT[F[_], E, A](a: => A)(implicit F: Applicative[F]): ValidationT[F, E, A] =
    ValidationT(F.point(Validation.success(a)))

  def failureT[F[_], E, A](e: => E)(implicit F: Applicative[F]): ValidationT[F, E, A] =
    ValidationT(F.point(Validation.failure(e)))

  def fromEitherT[F[_], E, A](e: EitherT[F, E, A])(implicit F: Functor[F]): ValidationT[F, E, A] =
    validationT(F.map(e.run)(x => Validation.fromEither(x.toEither)))
}


private trait ValidationTFoldable[F[_], E] extends Foldable.FromFoldr[ValidationT[F, E, ?]] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: ValidationT[F, E, A], z: => B)(f: (A, => B) => B) =
    fa.foldRight(z)(f)
}

private trait ValidationTFunctor[F[_], E] extends Functor[ValidationT[F, E, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: ValidationT[F, E, A])(f: A => B) =
    fa.map(f)
}

private trait ValidationTApply[F[_], E] extends Apply[ValidationT[F, E, ?]] with ValidationTFunctor[F, E] {
  implicit def F: Apply[F]
  implicit def E: Semigroup[E]

  override def ap[A, B](fa: => ValidationT[F, E, A])(f: => ValidationT[F, E, A => B]) =
    fa.ap(f)
}

private trait ValidationTApplicative[F[_], E] extends Applicative[ValidationT[F, E, ?]] with ValidationTApply[F, E] {
  implicit def F: Applicative[F]
  implicit def E: Semigroup[E]

  override def point[A](a: => A) =
    ValidationT.successT(a)
}

private trait ValidationTMonad[F[_], E] extends Monad[ValidationT[F, E, ?]] with ValidationTApplicative[F, E] {
  implicit def F: Monad[F]

  override final def bind[A, B](fa: ValidationT[F, E, A])(f: A => ValidationT[F, E, B]) =
    fa.flatMap(f)
}

private trait ValidationTTraverse[F[_], E] extends Traverse[ValidationT[F, E, ?]] with ValidationTFoldable[F, E] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: ValidationT[F, E, A])(f: A => G[B]) =
    fa.traverse(f)
}

private trait ValidationTBifunctor[F[_]] extends Bifunctor[ValidationT[F, ?, ?]] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: ValidationT[F, A, B])(f: A => C, g: B => D) =
    fab.bimap(f, g)
}

private trait ValidationTBitraverse[F[_]] extends Bitraverse[ValidationT[F, ?, ?]] with ValidationTBifunctor[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: ValidationT[F, A, B])(f: A => G[C], g: B => G[D]) =
    fab.bitraverse(f, g)
}

private trait ValidationTInstances6 {
  implicit def validationTFunctor[F[_], E](implicit F0: Functor[F]): Functor[ValidationT[F, E, ?]] =
    new ValidationTFunctor[F, E] {
      implicit def F = F0
    }
}

private trait ValidationTInstances5 extends ValidationTInstances6 {
  implicit def validationTFoldable[F[_], E](implicit F0: Foldable[F]): Foldable[ValidationT[F, E, ?]] =
    new ValidationTFoldable[F, E] {
      implicit def F = F0
    }

  implicit def validationTApply[F[_], E](implicit F0: Apply[F], E0: Semigroup[E]): Apply[ValidationT[F, E, ?]] =
    new ValidationTApply[F, E] {
      implicit def F = F0
      implicit def E = E0
    }
}

private trait ValidationTInstances4 extends ValidationTInstances5 {
  implicit def validationTTraverse[F[_], E](implicit F0: Traverse[F]): Traverse[ValidationT[F, E, ?]] =
    new ValidationTTraverse[F, E] {
      implicit def F = F0
    }
}

private trait ValidationTInstances3 extends ValidationTInstances4 {
  implicit def validationTApplicative[F[_], E](implicit F0: Applicative[F], E0: Semigroup[E]): Applicative[ValidationT[F, E, ?]] =
    new ValidationTApplicative[F, E] {
      implicit def F = F0
      implicit def E = E0
    }
}

private trait ValidationTInstances2 extends ValidationTInstances3 {
  implicit def validationTMonad[F[_], E](implicit F0: Monad[F], E0: Semigroup[E]): Monad[ValidationT[F, E, ?]] =
    new ValidationTMonad[F, E] {
      implicit def F = F0
      implicit def E = E0
    }
}

private trait ValidationTInstances1 extends ValidationTInstances2 {
  implicit def validationTBifunctor[F[_]](implicit F0: Functor[F]): Bifunctor[ValidationT[F, ?, ?]] =
    new ValidationTBifunctor[F] {
      implicit def F = F0
    }
}

private trait ValidationTInstances0 extends ValidationTInstances1 {
  implicit def validationTBitraverse[F[_]](implicit F0: Traverse[F]): Bitraverse[ValidationT[F, ?, ?]] =
    new ValidationTBitraverse[F] {
      implicit def F = F0
    }
}

private trait ValidationTInstances extends ValidationTInstances0 {
  implicit def validationTEqual[F[_], E, A](implicit F: Equal[F[Validation[E, A]]]): Equal[ValidationT[F, E, A]] =
    F.contramap(_.run)

  implicit def validationTSemigroup[F[_], E, A](implicit F: Semigroup[F[Validation[E, A]]]): Semigroup[ValidationT[F, E, A]] =
    F.xmap(ValidationT(_), _.run)

  implicit def validationTShow[F[_], E, A](implicit F: Show[F[Validation[E, A]]]): Show[ValidationT[F, E, A]] =
    F.contramap(_.run)
}

private [typify] object ValidationT extends ValidationTFunctions with ValidationTInstances
