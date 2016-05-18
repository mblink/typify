package typify

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.{BooleanOperators, ExtendedBoolean, forAllNoShrink}
import scala.reflect.ClassTag
import scalaz.std.option._
import scalaz.syntax.std.boolean._

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
    implicit case object MPP extends MustParse[P]
    implicit case object MPOP extends MustParse[Option[P]]
  }

  def make[A](k: String, v: A)(implicit mp: implicits.MustParse[A]): P
  def to[A](v: A)(implicit mp: implicits.MustParse[A]): P
}

class CanParseProp[P](mp: MakeParsed[P])(implicit
   cpp: CanParse[P, P], cpop: CanParse[Option[P], P],
   cpi: CanParse[Int, P], cpoi: CanParse[Option[Int], P],
   cps: CanParse[String, P], cpos: CanParse[Option[String], P],
   cpl: CanParse[Long, P], cpol: CanParse[Option[Long], P],
   cti: ClassTag[Int], ctoi: ClassTag[Option[Int]], cts: ClassTag[String],
   ctos: ClassTag[Option[String]], ctl: ClassTag[Long], ctol: ClassTag[Option[Long]],
   ctp: ClassTag[P], ctop: ClassTag[Option[P]]) {
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
    (((cp.parse(k, mp.make(k, some(g))).toOption == some(some(g))) :|
      s"some[$l] parses valid") &&
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

  def recursive =
    forAllNoShrink { (k: NEString, i: Int, s: String, l: Long) =>
      val nested = mp.make(k, s)
      val bnested = mp.make(k, i)
      // P
      assert("P", k, cpp, nested, bnested, true) &&
      // Option[P]
      assertO("P", k, cpop, nested, bnested, true)
    }

  def apply = int && string && long && recursive
}