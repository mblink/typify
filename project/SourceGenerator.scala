package typify

trait SourceGenerator {
  def apply(): String
}

object SourceGenerator {
  val tpes = List("Int", "String", "Boolean")
  val tupleLimit = 12

  case object Util extends SourceGenerator {
    def apply(): String = s"""
package typify.test

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.propBoolean
import typify.record._
import typify.tuple._

object Util {
  def compare(expected: Any, actual: Any): Prop = (actual == expected) :| s"Expected: $$expected, actual: $$actual"

  implicit def labelledArb[K, V](implicit V: Arbitrary[V]): Arbitrary[K ->> V] = V.asInstanceOf[Arbitrary[K ->> V]]

  private implicit val emptyTupleArb: Arbitrary[EmptyTuple] = Arbitrary(Gen.const(EmptyTuple))

  private implicit def tupleConsArb[H: Arbitrary, T <: Tuple: Arbitrary]: Arbitrary[H *: T] =
    Arbitrary(for {
      h <- Arbitrary.arbitrary[H]
      t <- Arbitrary.arbitrary[T]
    } yield h *: t)

  ${1.to(tupleLimit).map { i =>
    val tupleTpe = 1.to(i).foldRight("EmptyTuple")(
      (j, acc) => s"""("k$j" ->> ${tpes(j % tpes.length)}) *: $acc""")
    s"""
  type X$i = $tupleTpe
  implicit val x${i}Arb: Arbitrary[X$i] = tupleConsArb
"""
  }.mkString("\n")}
}
"""
  }

  case object TupleSelectorTest extends SourceGenerator {
    def apply(): String = s"""
package typify.tuple

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import typify.record._
import typify.test.Util._

object TupleSelectorTest extends Properties("TupleSelector") {
  ${1.to(tupleLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = s""""k$j" ->> ${tpes(j % tpes.length)}"""
      s"""

  property.update("select ${j - 1} from Tuple$i", Prop.forAll { (x: X$i) =>
    compare(x.toList.apply(${j - 1}), TupleSelector[X$i, $tpe].apply(x))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object SelectorTest extends SourceGenerator {
    def apply(): String = s"""
package typify.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import typify.test.Util._

object SelectorTest extends Properties("Selector") {
  ${1.to(tupleLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      s"""

  property.update("select k$j from Tuple$i", Prop.forAll { (x: X$i) =>
    compare(x.toList.apply(${j - 1}), Selector[X$i, "k$j"].apply(x))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object UpdaterTest extends SourceGenerator {
    def apply(): String = s"""
package typify.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import typify.test.Util._

object UpdaterTest extends Properties("Updater") {
  ${1.to(tupleLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = tpes(j % tpes.length)
      s"""

  property.update("update k$j in Tuple$i", Prop.forAll { (x: X$i, a: $tpe) =>
    val upd = Updater[X$i, "k$j" ->> $tpe].apply(x, label["k$j"](a))
    compare(a, upd.toList.apply(${j - 1}))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object ModifierTest extends SourceGenerator {
    def apply(): String = s"""
package typify.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import typify.test.Util._

object ModifierTest extends Properties("Modifier") {
  ${1.to(tupleLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = tpes(j % tpes.length)
      val nextTpe = tpes((j + 1) % tpes.length)
      s"""

  property.update("modify k$j in Tuple$i", Prop.forAll { (x: X$i, a: $nextTpe) =>
    val mod = Modifier[X$i, "k$j", $tpe, $nextTpe].apply(x, _ => a)
    compare(a, mod.toList.apply(${j - 1}))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object RenamerTest extends SourceGenerator {
    def apply(): String = s"""
package typify.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import typify.test.Util._

object RenamerTest extends Properties("Renamer") {
  ${1.to(tupleLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = tpes(j % tpes.length)
      val nextTpe = tpes((j + 1) % tpes.length)
      s"""

  property.update("rename k$j in Tuple$i", Prop.forAll { (x: X$i, k: String) =>
    val renamed = Renamer[X$i, "k$j", k.type].apply(x)
    compare(renamed.toList.apply(${j - 1}), Selector[renamed.type, k.type].apply(renamed))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object RemoverTest extends SourceGenerator {
    def apply(): String = s"""
package typify.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import typify.test.Util._
import typify.tuple.EmptyTuple

object RemoverTest extends Properties("Remover") {
  ${1.to(tupleLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = tpes(j % tpes.length)
      val nextTpe = tpes((j + 1) % tpes.length)
      s"""

  property.update("remove k$j from Tuple$i", Prop.forAll { (x: X$i) =>
    val removed = Remover[X$i, "k$j"].apply(x)._2
    val removedL = removed.toList
    (true: Prop)${if ((1.to(j - 1) ++ (j + 1).to(i)).nonEmpty) " &&" else " && removedL.isEmpty && compare(removed, EmptyTuple)"}
      ${(
        1.to(j - 1).map(k => s"""compare(removedL(${k - 1}), Selector[removed.type, "k$k"].apply(removed))""") ++
        (j + 1).to(i).map(k => s"""compare(removedL(${k - 2}), Selector[removed.type, "k$k"].apply(removed))""")
      ).mkString(" &&\n      ")}
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }
}
