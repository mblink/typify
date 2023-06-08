package typify.tuple

type IfEq[A, B, IfTrue, IfFalse] <: IfTrue | IfFalse = A match {
  case B => IfTrue
  case _ => IfFalse
}
