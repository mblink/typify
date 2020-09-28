package typify

import shapeless.HList
import shapeless.ops.record.Selector
import shapeless.tag.@@

/** Evidence that a value of type `A` contains a value of type `Out` at a typed key `K`
  * i.e. a `shapeless.ops.record.Selector[A, K]` without the constraint that `A` is an `HList`
  *
  * @tparam A The type of the containing value
  * @tparam K The type of the key
  */
trait KLens[A, K] {
  type Out
  def apply(a: A): Out
}

object KLens {
  /** A type alias that lifts the type member `Out` into a type parameter
    * so the compiler can properly infer the output type of an implicit field
    *
    * @see https://gigiigig.github.io/posts/2015/09/13/aux-pattern.html
    */
  type Aux[A, K, O] = KLens[A, K] { type Out = O }

  private final class Inst[K](val dummy: Boolean = true) extends AnyVal {
    def apply[A, O](f: A => O): Aux[A, K, O] = new KLens[A, K] {
      type Out = O
      def apply(a: A): O = f(a)
    }
  }

  private def inst[K] = new Inst[K]

  /** Derive a `KLens[L, K]` given a `Selector[L, K]` provided by shapeless if `L` is a record containing `K` */
  implicit def kLensFromSelector[A <: HList, K](implicit s: Selector[A, K]): Aux[A, K, s.Out] =
    inst[K](s(_: A))

  /** Derive a `KLens[A, K]` given a generic representation of `A` as the record `Repr` and a `Selector[Repr, K]` */
  implicit def kLensFromReprSelector[A, Repr <: HList, K](
    implicit lg: StringLabelledGeneric.Aux[A, Repr],
    s: Selector[Repr, K]
  ): Aux[A, K, s.Out] =
    inst[K]((a: A) => s(lg.to(a)))

  /** Derive a `KLens[L, String @@ K]` given a `KLens[L, K]` -- used for `selectDynamic` */
  implicit def kLensForTaggedString[A, K](implicit f: KLens[A, K]): Aux[A, String @@ K, f.Out] =
    inst[String @@ K](f(_: A))
}
