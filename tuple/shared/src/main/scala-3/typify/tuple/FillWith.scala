package typify.tuple

import scala.compiletime.summonAll

trait FillWith[F, L <: Tuple] extends DepFn0 { final type Out = L }

object FillWith {
  inline def apply[F, L <: Tuple](using f: FillWith[F, L]): FillWith[F, L] = f

  inline given fillWithInst[F <: Poly, L <: Tuple]: FillWith[F, L] =
    new FillWith[F, L] {
      def apply(): L =
        Tuple.fromArray(summonAll[Tuple.Map[L, Case0[F, *]]]
          .toArray.map(_.asInstanceOf[Case0[F, Any]].run())).asInstanceOf[L]
    }
}
