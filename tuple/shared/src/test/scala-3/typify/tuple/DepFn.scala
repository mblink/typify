package typify.tuple

/** Polymorphic identity function */
object id extends Poly1 {
  given apply[T]: Case.Aux[T, T] = at(identity)
}

/** Polymorphic singleton function. */
object singleton extends Poly1 {
  given apply[T]: Case.Aux[T, Set[T]] = at(Set(_))
}

/** Polymorphic function selecting an arbitrary element from a non-empty `Set`. */
object choose extends Poly1 {
  given apply[T]: Case.Aux[Set[T], Option[T]] = at(_.headOption)
}

/** Polymorphic function creating singleton `List`s. */
object list extends Poly1 {
  given apply[T]: Case.Aux[T, List[T]] = at(List(_))
}

/** Polymorphic function returning the head of a `List`. */
object headOption extends Poly1 {
  given apply[T]: Case.Aux[List[T], Option[T]] = at(_.headOption)
}

/** Polymorphic function which injects a value into an `Option`. */
object option extends Poly1 {
  given apply[T]: Case.Aux[T, Option[T]] = at(Option(_))
}

/** Polymorphic function testing whether or not an `Option` is defined. */
object isDefined extends Poly1 {
  given apply[T]: Case.Aux[Option[T], Boolean] = at(_.isDefined)
}

/** Polymorphic function which opens an `Option`. */
object get extends Poly1 {
  given apply[T]: Case.Aux[Option[T], T] = at(_.get)
}
