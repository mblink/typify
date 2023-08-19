package typify.tuple

type ConstMapperT[C, T <: Tuple] <: Tuple = T match {
  case EmptyTuple => EmptyTuple
  case _ *: t => C *: ConstMapperT[C, t]
}

trait ConstMapper[C, T <: Tuple] extends DepFn2[C, T] { type Out <: Tuple }

object ConstMapper {
  type Aux[C, T <: Tuple, O <: Tuple] = ConstMapper[C, T] { type Out = O }

  inline def apply[C, T <: Tuple](using m: ConstMapper[C, T]): ConstMapper.Aux[C, T, m.Out] = m

  private def constMap[C, T <: Tuple](c: C, t: T): ConstMapperT[C, T] =
    t match {
      case _: EmptyTuple => EmptyTuple
      case x: (_ *: t) => c *: constMap[C, t](c, x.tail)
    }

  given constMapperTuple[C, T <: Tuple]: ConstMapper.Aux[C, T, ConstMapperT[C, T]] =
    new ConstMapper[C, T] {
      type Out = ConstMapperT[C, T]
      def apply(c: C, t: T): Out = constMap[C, T](c, t)
    }
}
