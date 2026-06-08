package typify

import cats.data.NonEmptyList
import cats.syntax.option._
import cats.syntax.validated._
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}
import scala.util.chaining._

trait MakeParsed[P] {

  object implicits {
    sealed trait MustParse[A] {
      def apply(a: A): A = a
    }
    implicit case object MPS extends MustParse[String]
    implicit case object MPOS extends MustParse[Option[String]]
    implicit case object MPI extends MustParse[Int]
    implicit case object MPOI extends MustParse[Option[Int]]
    implicit case object MPL extends MustParse[Long]
    implicit case object MPOL extends MustParse[Option[Long]]
    implicit case object MPD extends MustParse[Double]
    implicit case object MPOD extends MustParse[Option[Double]]
    implicit case object MPB extends MustParse[Boolean]
    implicit case object MPOB extends MustParse[Option[Boolean]]
    implicit case object MPLI extends MustParse[List[Int]]
    implicit case object MPOLI extends MustParse[Option[List[Int]]]
    implicit case object MPLS extends MustParse[List[String]]
    implicit case object MPOLS extends MustParse[Option[List[String]]]
    implicit case object MPP extends MustParse[P]
    implicit case object MPOP extends MustParse[Option[P]]
    implicit case object MPLP extends MustParse[List[P]]
  }

  def make[A](k: String, v: A)(implicit mp: implicits.MustParse[A]): Cursor[P]
  def to[A](v: A)(implicit mp: implicits.MustParse[A]): Cursor[P]
}

class CanParseProp[P](mp: MakeParsed[P])(implicit
   cpp: CanParse[P, P], cpop: CanParse[Option[P], P],
   cpi: CanParse[Int, P], cpoi: CanParse[Option[Int], P],
   cps: CanParse[String, P], cpos: CanParse[Option[String], P],
   cpl: CanParse[Long, P], cpol: CanParse[Option[Long], P],
   cpd: CanParse[Double, P], cpod: CanParse[Option[Double], P],
   cpb: CanParse[Boolean, P], cpob: CanParse[Option[Boolean], P],
   cpli: CanParse[List[Int], P], cpoli: CanParse[Option[List[Int]], P]) {
  import mp.implicits._

  def matches[A](msg: String)(actual: A, expected: A): Prop =
    (expected == actual) :| s"$msg\n\nEXPECTED:\n  ${expected}\n\nACTUAL:\n  ${actual}"

  private def assert[A, B](l: String, k: String, cp: CanParse[A, P], g: A, b: B, rc: Boolean = false)(
      implicit mpa: MustParse[A], mpb: MustParse[B]): Prop =
    matches(s"$l parses valid")(cp.parse(k, mp.make(k, g)).toOption, g.some) &&
    matches(s"$l missed key")(cp.parse(k + "a", mp.make(k, g)).toOption, none[A]) &&
    matches(s"$l wrong type")(cp.parse(k, mp.make(k, b)).toOption, if (rc) b.some else none[A]) &&
    matches(s"$l represents valid")(cp(mp.to(g)).toOption, g.some) &&
    matches(s"$l represented wrong type")(cp(mp.to(b)).toOption, if (rc) b.some else none[A])

  private def assertO[A, B](l: String, k: String, cp: CanParse[Option[A], P], g: A, b: B, rc: Boolean = false)(
      implicit mpoa: MustParse[Option[A]], mpob: MustParse[Option[B]]): Prop =
    matches(s"some[$l] parses valid")(cp.parse(k, mp.make(k, g.some)).toOption, g.some.some) &&
    matches(s"none[$l] parses valid")(cp.parse(k + "a", mp.make(k, g.some)).toOption, none[A].some) &&
    matches(s"some[$l] missed key")(cp.parse(k, mp.make(k, b.some)).toOption, if (rc) b.some.some else none[Option[A]]) &&
    matches(s"some[$l] wrong type")(cp.parse(k, mp.make(k, b.some)).toOption, if (rc) b.some.some else none[Option[A]]) &&
    matches(s"$l represents valid")(cp(mp.to(g.some)).toOption, g.some.some) &&
    matches(s"none[$l] represents valid")(cp(mp.to(none[A])).toOption, none[A].some) &&
    matches(s"$l repesented wrong type")(cp(mp.to(b.some)).toOption, if (rc) b.some.some else none[Option[A]])

  type NEString = String
  implicit val arbNE: Arbitrary[NEString] = Arbitrary { Gen.alphaStr.suchThat(_.nonEmpty) }

  def int =
    forAllNoShrink { (k: NEString, i: Int, s: String) =>
      // Int
      assert("Int", k, cpi, i, s) &&
      ((cpi.parse(k, mp.make(k, i.toString)).toOption == i.some) :|
       s"Int parses stringified") &&
      ((cpi(mp.to(i.toString)).toOption == i.some) :|
       "Int represents stringified") &&
      // Option[Int]
      assertO("Int", k, cpoi, i, s) &&
      ((cpoi.parse(k, mp.make(k, i.toString.some)).toOption == i.some.some) :|
       "some[Int] parses stringified") &&
      ((cpoi(mp.to(i.toString.some)).toOption == i.some.some) :|
       "some[Int] represents stringified")
    }

  def string =
    forAllNoShrink { (k: NEString, i: Int, s: String) =>
      // String
      assert("String", k, cps, s, i) &&
      // Option[String]
      assertO("String", k, cpos, s, i)
    }

  def long =
    forAllNoShrink { (k: NEString, s: String, l: Long) =>
      // Long
      assert("Long", k, cpl, l, s) &&
      ((cpl.parse(k, mp.make(k, l.toString)).toOption == l.some) :|
       "Long parses stringified") &&
      ((cpl(mp.to(l.toString)).toOption == l.some) :|
       "Long represents stringified") &&
      // Option[Long]
      assertO("Long", k, cpol, l, s) &&
      ((cpol.parse(k, mp.make(k, l.toString.some)).toOption == l.some.some) :|
       "some[Long] parses stringified") &&
      ((cpol(mp.to(l.toString.some)).toOption == l.some.some) :|
       "some[Long] represents stringified")
    }

  def double =
    forAllNoShrink { (k: NEString, s: String, d: Double) =>
      // Double
      assert("Double", k, cpd, d, s) &&
      ((cpd.parse(k, mp.make(k, d.toString)).toOption == d.some) :|
       "Double parses stringified") &&
      ((cpd(mp.to(d.toString)).toOption == d.some) :|
       "Double represents stringified") &&
      // Option[Double]
      assertO("Double", k, cpod, d, s) &&
      ((cpod.parse(k, mp.make(k, d.toString.some)).toOption == d.some.some) :|
       "some[Double] parses stringified") &&
      ((cpod(mp.to(d.toString.some)).toOption == d.some.some) :|
       "some[Double] represents stringified")
    }

  def boolean =
    forAllNoShrink { (k: NEString, i: Int, b: Boolean) =>
      // Boolean
      assert("Boolean", k, cpb, b, i) &&
      ((cpb.parse(k, mp.make(k, b.toString)).toOption == b.some) :|
       "Boolean parses stringified") &&
      ((cpb(mp.to(b.toString)).toOption == b.some) :|
       "Boolean represents stringified") &&
      // Option[Boolean]
      assertO("Boolean", k, cpob, b, i) &&
      ((cpob.parse(k, mp.make(k, b.toString.some)).toOption == b.some.some) :|
       "Boolean parses stringified") &&
      ((cpob(mp.to(b.toString.some)).toOption == b.some.some) :|
       "Boolean represents stringified")
    }

  def stringify[A](l: String, cp: CanParse[A, P])(implicit A: Arbitrary[A]) =
    forAllNoShrink { (k: NEString, a: A) =>
      (((cp.parse(k, mp.make(k, a.toString)).toOption == a.some) :|
        s"Cannot parse value of type $l from string") &&
      ((cp(mp.to(a.toString)).toOption == a.some) :|
        s"Cannot cast string to type $l"))
    }

  def boolString = stringify[Boolean]("Boolean", cpb)
  def intString = stringify[Int]("Int", cpi)
  def longString = stringify[Long]("Long", cpl)
  def doubleString = stringify[Double]("Double", cpd)

  implicit def arbNEL[A](implicit aa: Arbitrary[A]): Arbitrary[NonEmptyList[A]] =
    Arbitrary(for {
      h <- aa.arbitrary
      t <- Arbitrary.arbitrary[List[A]]
    } yield NonEmptyList(h, t))

  def list =
    forAllNoShrink { (k: NEString, li0: NonEmptyList[Int], ls0: NonEmptyList[String]) =>
      val (li, ls) = (li0.toList, ls0.toList)
      // List[Int]
      assert("List[Int]", k, cpli, li, ls) &&
      ((cpli.parse(k, mp.make(k, li.map(_.toString))).toOption == li.some) :|
       "List[Int] parses stringified") &&
      ((cpli(mp.to(li.map(_.toString))).toOption == li.some) :|
       "List[Int] represents stringified") &&
      // Option[List[Int]]
      assertO("List[Int]", k, cpoli, li, ls) &&
      ((cpoli.parse(k, mp.make(k, li.map(_.toString).some)).toOption == li.some.some) :|
       "some[List[Int]] parses stringified") &&
      ((cpoli(mp.to(li.map(_.toString).some)).toOption == li.some.some) :|
       "some[List[Int]] represents stringified") &&
      // List[P]
      ((parseList[P, Unit, Int](mp.to(li.flatMap(i => mp.make(k, i).focus)))(
        _ => ().invalidNel[List[Int]],
        cpi.parse(k, _).leftMap(_.map(_ => ()))).toOption == li.some) :|
      "List[Int] parses via List[P]")
    }

  def recursive =
    forAllNoShrink { (k: NEString, i: Int, s: String) =>
      val nested = mp.make(k, s)
      val bnested = mp.make(k, i)
      // P
      assert("P", k, cpp, nested.focus.get, bnested.focus.get, true) &&
      // Option[P]
      assertO("P", k, cpop, nested.focus.get, bnested.focus.get, true)
    }

  def props(msg: String): Properties = new Properties(msg) {
    property.update("int", int).pipe(_ => ())
    property.update("string", string).pipe(_ => ())
    property.update("long", long).pipe(_ => ())
    property.update("double", double).pipe(_ => ())
    property.update("boolean", boolean).pipe(_ => ())
    property.update("list", list).pipe(_ => ())
    property.update("recursive", recursive).pipe(_ => ())
    property.update("boolString", boolString).pipe(_ => ())
    property.update("intString", intString).pipe(_ => ())
    property.update("longString", longString).pipe(_ => ())
    property.update("doubleString", doubleString).pipe(_ => ())
  }
}
