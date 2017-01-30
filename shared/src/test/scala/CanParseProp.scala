package typify

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.{BooleanOperators, ExtendedBoolean, forAllNoShrink}
import scala.reflect.ClassTag
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
   cpb: CanParse[Boolean, P], cpob: CanParse[Option[Boolean], P],
   cti: ClassTag[Int], ctoi: ClassTag[Option[Int]], cts: ClassTag[String],
   ctos: ClassTag[Option[String]], ctl: ClassTag[Long], ctol: ClassTag[Option[Long]],
   ctb: ClassTag[Boolean], ctob: ClassTag[Option[Boolean]],
   cpli: CanParse[List[Int], P], cpoli: CanParse[Option[List[Int]], P],
   cplp: CanParse[List[P], P], ctp: ClassTag[P], ctop: ClassTag[Option[P]]) {
  import mp.implicits._

  def assert[A: ClassTag, B: ClassTag](l: String, k: String, cp: CanParse[A, P],
                                       g: A, b: B, rc: Boolean = false)(implicit
    mpa: MustParse[A], mpb: MustParse[B]): Prop =
    (((cp.parse(k, mp.make(k, g)).toOption == some(g)) :|
      s"$l parses valid") &&
     ((cp.parse(k + "a", mp.make(k, g)).toOption == none[A]) :|
      s"$l missed key") &&
     ((cp.parse(k, mp.make(k, b)).toOption == rc.fold(some(b), none[A])) :|
      s"$l wrong type") &&
     ((cp.as(mp.to(g)).toOption == some(g)) :|
      s"$l represents valid") &&
     ((cp.as(mp.to(b)).toOption == rc.fold(some(b), none[A])) :|
      s"$l repesented wrong type"))

  def assertO[A, B](l: String, k: String, cp: CanParse[Option[A], P],
                   g: A, b: B, rc: Boolean = false)(implicit
    mpoa: MustParse[Option[A]], mpob: MustParse[Option[B]],
    ctoa: ClassTag[Option[A]], ctob: ClassTag[Option[B]]): Prop =
    ({ val a = cp.parse(k, mp.make(k, some(g)))
    ((cp.parse(k, mp.make(k, some(g))).toOption == some(some(g))) :|
      s"some[$l] parses valid: get ${a} ex ${some(some(g))}") } &&
     ((cp.parse(k, mp.make(k, none[A])).toOption == some(none[A])) :|
      s"none[$l] parses valid") &&
     ((cp.parse(k + "a", mp.make(k, some(g))).toOption == some(none[A])) :|
      s"some[$l] missed key") &&
     ((cp.parse(k, mp.make(k, some(b))).toOption == rc.fold(some(some(b)), none[Option[A]])) :|
      s"some[$l] wrong type") &&
     ((cp.as(mp.to(some(g))).toOption == some(some(g))) :|
      s"$l represents valid") &&
     ((cp.as(mp.to(none[A])).toOption == some(none[A])) :|
      s"none[$l] represents valid") &&
     ((cp.as(mp.to(some(b))).toOption == rc.fold(some(some(b)), none[Option[A]])) :|
      s"$l repesented wrong type"))

  type NEString = String
  implicit val arbNE: Arbitrary[NEString] = Arbitrary { Gen.alphaStr.suchThat(_.nonEmpty) }

  def int =
    forAllNoShrink { (k: NEString, i: Int, s: String, l: Long) =>
      // Int
      assert("Int", k, cpi, i, s) &&
      ((cpi.parse(k, mp.make(k, i.toString)).toOption == some(i)) :|
       s"Int parses stringified") &&
      ((cpi.as(mp.to(i.toString)).toOption == some(i)) :|
       "Int represents stringified") &&
      // Option[Int]
      assertO("Int", k, cpoi, i, s) &&
      ((cpoi.parse(k, mp.make(k, some(i.toString))).toOption == some(some(i))) :|
       "some[Int] parses stringified") &&
      ((cpoi.as(mp.to(some(i.toString))).toOption == some(some(i))) :|
       "some[Int] represents stringified")
    }

  def string =
    forAllNoShrink { (k: NEString, i: Int, s: String, l: Long) =>
      // String
      assert("String", k, cps, s, i) &&
      // Option[String]
      assertO("String", k, cpos, s, i)
    }

  def long =
    forAllNoShrink { (k: NEString, i: Int, s: String, l: Long) =>
      // Long
      assert("Long", k, cpl, l, s) &&
      ((cpl.parse(k, mp.make(k, l.toString)).toOption == some(l)) :|
       "Long parses stringified") &&
      ((cpl.as(mp.to(l.toString)).toOption == some(l)) :|
       "Long represents stringified") &&
      // Option[Long]
      assertO("Long", k, cpol, l, s) &&
      ((cpol.parse(k, mp.make(k, some(l.toString))).toOption == some(some(l))) :|
       "some[Long] parses stringified") &&
      ((cpol.as(mp.to(some(l.toString))).toOption == some(some(l))) :|
       "some[Long] represents stringified")
    }

  def boolean =
    forAllNoShrink { (k: NEString, i: Int, b: Boolean) =>
      // Boolean
      assert("Boolean", k, cpb, b, i) &&
      // Option[Boolean]
      assertO("Boolean", k, cpob, b, i)
    }

  type NEList[A] = List[A]
  implicit def arbNEL[A](implicit aa: Arbitrary[A]) =
    Arbitrary { Arbitrary.arbitrary[List[A]].suchThat(_.nonEmpty) }

  def list =
    forAllNoShrink { (k: NEString, li: NEList[Int], ls: NEList[String], ll: NEList[Long]) =>
      // List[Int]
      assert("List[Int]", k, cpli, li, ls) &&
      ((cpli.parse(k, mp.make(k, li.map(_.toString))).toOption == some(li)) :|
       "List[Int] parses stringified") &&
      ((cpli.as(mp.to(li.map(_.toString))).toOption == some(li)) :|
       "List[Int] represents stringified") &&
      // Option[List[Int]]
      assertO("List[Int]", k, cpoli, li, ls) &&
      ((cpoli.parse(k, mp.make(k, some(li.map(_.toString)))).toOption == some(some(li))) :|
       "some[List[Int]] parses stringified") &&
      ((cpoli.as(mp.to(some(li.map(_.toString)))).toOption == some(some(li))) :|
       "some[List[Int]] represents stringified") &&
      // List[P]
      ((cplp.as(mp.to(li.map(i => mp.make(k, i))))
        .disjunction
        .flatMap(_.map(cpi.parse(k, _)).sequenceU.disjunction)
        .toOption == some(li)) :|
       "List[Int] parses via List[P]")
    }

  def recursive =
    forAllNoShrink { (k: NEString, i: Int, s: String, l: Long) =>
      val nested = mp.make(k, s)
      val bnested = mp.make(k, i)
      // P
      assert("P", k, cpp, nested, bnested, true) &&
      // Option[P]
      assertO("P", k, cpop, nested, bnested, true)
    }

  def apply = int && string && long && boolean && list && recursive
}
