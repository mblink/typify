package typify

import cats.{Bimonad, CommutativeMonad, Distributive, Eq, NonEmptyTraverse}
import cats.syntax.eq._

opaque type Tagged[T, A] = A
type @@[T, A] = Tagged[T, A]

val tag: Tagged.type = Tagged

type Labelled[T, A] = Tagged[T, A]
val Labelled: Tagged.type = Tagged
type ->>[T, A] = Tagged[T, A]

inline def label[T]: Tagged.PartialAp[T] = new Tagged.PartialAp[T]

extension [T, A](x: Tagged[T, A])
  def value: A = x
  def tag(implicit t: ValueOf[T]): T = t.value

sealed trait TaggedLP {
  inline implicit def eqForTaggedSingleton[A <: Singleton, T <: String](implicit t: ValueOf[T]): Eq[Tagged[T, A]] =
    Eq.instance((a1, a2) => a1.value == a2.value && a1.tag == a2.tag)
}

object Tagged extends TaggedLP {
  final class PartialAp[T](private val dummy: Boolean = false) extends AnyVal {
    inline def apply[A](a: A): Tagged[T, A] = a
  }

  inline def apply[T]: PartialAp[T] = new PartialAp[T]

  type TaggedInst[T] = Bimonad[Tagged[T, *]] & CommutativeMonad[Tagged[T, *]] & NonEmptyTraverse[Tagged[T, *]] & Distributive[Tagged[T, *]]

  inline implicit def instanceForTagged[T]: TaggedInst[T] = cats.catsInstancesForId.asInstanceOf[TaggedInst[T]]

  inline implicit def eqForTagged[A, T <: String](implicit e: Eq[A], t: ValueOf[T]): Eq[Tagged[T, A]] =
    Eq.instance((l1: Tagged[T, A], l2: Tagged[T, A]) => (l1.tag: String) === l2.tag && e.eqv(l1, l2))

  inline implicit def valueOfTagged[T, A](implicit a: ValueOf[A]): ValueOf[Tagged[T, A]] =
    new ValueOf[Tagged[T, A]](a.value)

  extension [T <: Singleton](t: T) {
    inline def ->>[A](a: A): Tagged[T, A] = a
  }
}
