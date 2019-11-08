package typify

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}
import scalaz.Validation.FlatMap._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.std.boolean._
import scalaz.syntax.traverse._

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

  def make[A](k: String, v: A)(implicit mp: implicits.MustParse[A]): P
  def to[A](v: A)(implicit mp: implicits.MustParse[A]): P
}

class CanParseProp[P](mp: MakeParsed[P])(implicit
   cpp: CanParse[P, P], cpop: CanParse[Option[P], P],
   cpi: CanParse[Int, P], cpoi: CanParse[Option[Int], P],
   cps: CanParse[String, P], cpos: CanParse[Option[String], P],
   cpl: CanParse[Long, P], cpol: CanParse[Option[Long], P],
   cpd: CanParse[Double, P], cpod: CanParse[Option[Double], P],
   cpb: CanParse[Boolean, P], cpob: CanParse[Option[Boolean], P],
   cpli: CanParse[List[Int], P], cpoli: CanParse[Option[List[Int]], P],
   cplp: CanParse[List[P], P]) {
  import mp.implicits._

  def assert[A, B](l: String, k: String, cp: CanParse[A, P],
                                       g: A, b: B, rc: Boolean = false)(implicit
    mpa: MustParse[A], mpb: MustParse[B]): Prop = {
    (((cp.parse(k, Parsed.init(mp.make(k, g))).toOption.map(_.value) == some(g)) :|
      s"$l parses valid") &&
     ((cp.parse(k + "a", Parsed.init(mp.make(k, g))).toOption.map(_.value) == none[A]) :|
      s"$l missed key") &&
     ((cp.parse(k, Parsed.init(mp.make(k, b))).toOption.map(_.value) == rc.fold(some(b), none[A])) :|
      s"$l wrong type") &&
     ((cp.as(Parsed.init(mp.to(g))).toOption.map(_.value) == some(g)) :|
      s"$l represents valid") &&
     ((cp.as(Parsed.init(mp.to(b))).toOption.map(_.value) == rc.fold(some(b), none[A])) :|
      s"$l repesented wrong type"))
  }

  def assertO[A, B](l: String, k: String, cp: CanParse[Option[A], P],
                   g: A, b: B, rc: Boolean = false)(implicit
    mpoa: MustParse[Option[A]], mpob: MustParse[Option[B]]): Prop =
    ({ val a = cp.parse(k, Parsed.init(mp.make(k, some(g))))
    ((cp.parse(k, Parsed.init(mp.make(k, some(g)))).toOption.map(_.value) == some(some(g))) :|
      s"some[$l] parses valid: get ${a} ex ${some(some(g))}") } &&
     ((cp.parse(k, Parsed.init(mp.make(k, none[A]))).toOption.map(_.value) == some(none[A])) :|
      s"none[$l] parses valid") &&
     ((cp.parse(k + "a", Parsed.init(mp.make(k, some(g)))).toOption.map(_.value) == some(none[A])) :|
      s"some[$l] missed key") &&
     ((cp.parse(k, Parsed.init(mp.make(k, some(b)))).toOption.map(_.value) == rc.fold(some(some(b)), none[Option[A]])) :|
      s"some[$l] wrong type") &&
     ((cp.as(Parsed.init(mp.to(some(g)))).toOption.map(_.value) == some(some(g))) :|
      s"$l represents valid") &&
     ((cp.as(Parsed.init(mp.to(none[A]))).toOption.map(_.value) == some(none[A])) :|
      s"none[$l] represents valid") &&
     ((cp.as(Parsed.init(mp.to(some(b)))).toOption.map(_.value) == rc.fold(some(some(b)), none[Option[A]])) :|
      s"$l repesented wrong type"))

  type NEString = String
  implicit val arbNE: Arbitrary[NEString] = Arbitrary { Gen.alphaStr.suchThat(_.nonEmpty) }

  def int =
    forAllNoShrink { (k: NEString, i: Int, s: String) =>
      // Int
      assert("Int", k, cpi, i, s) &&
      ((cpi.parse(k, Parsed.init(mp.make(k, i.toString))).toOption.map(_.value) == some(i)) :|
       s"Int parses stringified") &&
      ((cpi.as(Parsed.init(mp.to(i.toString))).toOption.map(_.value) == some(i)) :|
       "Int represents stringified") &&
      // Option[Int]
      assertO("Int", k, cpoi, i, s) &&
      ((cpoi.parse(k, Parsed.init(mp.make(k, some(i.toString)))).toOption.map(_.value) == some(some(i))) :|
       "some[Int] parses stringified") &&
      ((cpoi.as(Parsed.init(mp.to(some(i.toString)))).toOption.map(_.value) == some(some(i))) :|
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
      ((cpl.parse(k, Parsed.init(mp.make(k, l.toString))).toOption.map(_.value) == some(l)) :|
       "Long parses stringified") &&
      ((cpl.as(Parsed.init(mp.to(l.toString))).toOption.map(_.value) == some(l)) :|
       "Long represents stringified") &&
      // Option[Long]
      assertO("Long", k, cpol, l, s) &&
      ((cpol.parse(k, Parsed.init(mp.make(k, some(l.toString)))).toOption.map(_.value) == some(some(l))) :|
       "some[Long] parses stringified") &&
      ((cpol.as(Parsed.init(mp.to(some(l.toString)))).toOption.map(_.value) == some(some(l))) :|
       "some[Long] represents stringified")
    }

  def double =
    forAllNoShrink { (k: NEString, s: String, d: Double) =>
      // Double
      assert("Double", k, cpd, d, s) &&
      ((cpd.parse(k, Parsed.init(mp.make(k, d.toString))).toOption.map(_.value) == some(d)) :|
       "Double parses stringified") &&
      ((cpd.as(Parsed.init(mp.to(d.toString))).toOption.map(_.value) == some(d)) :|
       "Double represents stringified") &&
      // Option[Double]
      assertO("Double", k, cpod, d, s) &&
      ((cpod.parse(k, Parsed.init(mp.make(k, some(d.toString)))).toOption.map(_.value) == some(some(d))) :|
       "some[Double] parses stringified") &&
      ((cpod.as(Parsed.init(mp.to(some(d.toString)))).toOption.map(_.value) == some(some(d))) :|
       "some[Double] represents stringified")
    }

  def boolean =
    forAllNoShrink { (k: NEString, i: Int, b: Boolean) =>
      // Boolean
      assert("Boolean", k, cpb, b, i) &&
      ((cpb.parse(k, Parsed.init(mp.make(k, b.toString))).toOption.map(_.value) == some(b)) :|
       "Boolean parses stringified") &&
      ((cpb.as(Parsed.init(mp.to(b.toString))).toOption.map(_.value) == some(b)) :|
       "Boolean represents stringified") &&
      // Option[Boolean]
      assertO("Boolean", k, cpob, b, i) &&
      ((cpob.parse(k, Parsed.init(mp.make(k, some(b.toString)))).toOption.map(_.value) == some(some(b))) :|
       "Boolean parses stringified") &&
      ((cpob.as(Parsed.init(mp.to(some(b.toString)))).toOption.map(_.value) == some(some(b))) :|
       "Boolean represents stringified")
    }

  def stringify[A](l: String, cp: CanParse[A, P])(implicit A: Arbitrary[A]) =
    forAllNoShrink { (k: NEString, a: A) =>
      (((cp.parse(k, Parsed.init(mp.make(k, a.toString))).toOption.map(_.value) == some(a)) :|
        s"Cannot parse value of type $l from string") &&
      ((cp.as(Parsed.init(mp.to(a.toString))).toOption.map(_.value) == some(a)) :|
        s"Cannot cast string to type $l"))
    }

  def boolString = stringify[Boolean]("Boolean", cpb)
  def intString = stringify[Int]("Int", cpi)
  def longString = stringify[Long]("Long", cpl)
  def doubleString = stringify[Double]("Double", cpd)

  type NEList[A] = List[A]
  implicit def arbNEL[A](implicit aa: Arbitrary[A]) =
    Arbitrary { Arbitrary.arbitrary[List[A]].suchThat(_.nonEmpty) }

  def list =
    forAllNoShrink { (k: NEString, li: NEList[Int], ls: NEList[String]) =>
      // List[Int]
      assert("List[Int]", k, cpli, li, ls) &&
      ((cpli.parse(k, Parsed.init(mp.make(k, li.map(_.toString)))).toOption.map(_.value) == some(li)) :|
       "List[Int] parses stringified") &&
      ((cpli.as(Parsed.init(mp.to(li.map(_.toString)))).toOption.map(_.value) == some(li)) :|
       "List[Int] represents stringified") &&
      // Option[List[Int]]
      assertO("List[Int]", k, cpoli, li, ls) &&
      ((cpoli.parse(k, Parsed.init(mp.make(k, some(li.map(_.toString))))).toOption.map(_.value) == some(some(li))) :|
       "some[List[Int]] parses stringified") &&
      ((cpoli.as(Parsed.init(mp.to(some(li.map(_.toString))))).toOption.map(_.value) == some(some(li))) :|
       "some[List[Int]] represents stringified") &&
      // List[P]
      ((cplp.as(Parsed.init(mp.to(li.map(i => mp.make(k, i)))))
        .flatMap(p => p.value.traverse(x => cpi.parse(k, p.next(x))))
        .toOption.map(_.map(_.value)) == some(li)) :|
       "List[Int] parses via List[P]")
    }

  def recursive =
    forAllNoShrink { (k: NEString, i: Int, s: String) =>
      val nested = mp.make(k, s)
      val bnested = mp.make(k, i)
      // P
      assert("P", k, cpp, nested, bnested, true) &&
      // Option[P]
      assertO("P", k, cpop, nested, bnested, true)
    }

  def props(msg: String): Properties = new Properties(msg) {
    property("int") = int
    property("string") = string
    property("long") = long
    property("double") = double
    property("boolean") = boolean
    property("list") = list
    property("recursive") = recursive
    property("boolString") = boolString
    property("intString") = intString
    property("longString") = longString
    property("doubleString") = doubleString
  }
}
