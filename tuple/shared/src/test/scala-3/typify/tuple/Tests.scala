package typify
package tuple

import org.junit.Test
import org.junit.Assert._
import scala.reflect.TypeTest
import typify.test._
import typify.testutil._

class TupleTests {
  type SI = Set[Int] *: EmptyTuple
  type OI = Option[Int] *: EmptyTuple

  type III = Int *: Int *: Int *: EmptyTuple

  type SISS = Set[Int] *: Set[String] *: EmptyTuple
  type OIOS = Option[Int] *: Option[String] *: EmptyTuple

  type ISII = Int *: String *: Int *: Int *: EmptyTuple
  type IIII = Int *: Int *: Int *: Int *: EmptyTuple
  type IYII = Int *: Any *: Int *: Int *: EmptyTuple

  type OIOSOIOI = Option[Int] *: Option[String] *: Option[Int] *: Option[Int] *: EmptyTuple
  type SISSSISI = Set[Int] *: Set[String] *: Set[Int] *: Set[Int] *: EmptyTuple

  type BBBB = Boolean *: Boolean *: Boolean *: Boolean *: EmptyTuple

  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit
  case class Banana() extends Fruit

  type YYYY = Any *: Any *: Any *: Any *: EmptyTuple
  type FF = Fruit *: Fruit *: EmptyTuple
  type AP = Apple *: Pear *: EmptyTuple
  type BP = Banana *: Pear *: EmptyTuple
  type AF = Apple *: Fruit *: EmptyTuple
  type FFFF = Fruit *: Fruit *: Fruit *: Fruit *: EmptyTuple
  type APAP = Apple *: Pear *: Apple *: Pear *: EmptyTuple
  type APBP = Apple *: Pear *: Banana *: Pear *: EmptyTuple
  type APB = Apple *: Pear *: Banana *: EmptyTuple
  type PBPA = Pear *: Banana *: Pear *: Apple *: EmptyTuple
  type PABP = Pear *: Apple *: Banana *: Pear *: EmptyTuple

  // type APc = Apple :+: Pear :+: CNil
  // type ABPc = Apple :+: Banana :+: Pear :+: CNil

  val a: Apple = Apple()
  val p: Pear = Pear()
  val b: Banana = Banana()
  val f: Fruit = new Fruit {}

  val ap: AP = a *: p *: EmptyTuple
  val bp: BP = b *: p *: EmptyTuple
  val apap: APAP = a *: p *: a *: p *: EmptyTuple
  val apbp: APBP = a *: p *: b *: p *: EmptyTuple
  val apapList = List(a, p, a, p)
  val apbpList = List(a, p, b, p)
  val apapArray = Array(a, p, a, p)
  val apbpArray = Array(a, p, b, p)

  trait Ctv[-T]
  type CICSCICICD = Ctv[Int] *: Ctv[String] *: Ctv[Int] *: Ctv[Int] *: Ctv[Double] *: EmptyTuple

  val ci: Ctv[Int] = new Ctv[Int] {}
  val cs: Ctv[String] = new Ctv[String] {}
  val cd: Ctv[Double] = new Ctv[Double] {}
  val cicscicicdList = List(ci, cs, ci, ci, cd)
  val cicscicicdArray = Array(ci, cs, ci, ci, cd)
  val cicscicicd: CICSCICICD = ci *: cs *: ci *: ci *: cd *: EmptyTuple

  trait M[T]
  type MIMSMIMIMD = M[Int] *: M[String] *: M[Int] *: M[Int] *: M[Double] *: EmptyTuple

  val mi: M[Int] = new M[Int] {}
  val ms: M[String] = new M[String] {}
  val md: M[Double] = new M[Double] {}
  val mimsmimimdList = List(mi, ms, mi, mi, md)
  val mimsmimimdArray = Array(mi, ms, mi, mi, md)
  val mimsmimimd: MIMSMIMIMD = mi *: ms *: mi *: mi *: md *: EmptyTuple

  import language.existentials
  val mExist: M[_] = new M[Double] {}
  type MIMSMIMEMD = M[Int] *: M[String] *: M[Int] *: M[_] *: M[Double] *: EmptyTuple
  val mimsmimemdList = List(mi, ms, mi, mExist, md)
  val mimsmimemdArray = Array[M[_]](mi, ms, mi, mExist, md)
  val mimsmimemd: MIMSMIMEMD = mi *: ms *: mi *: mExist *: md *: EmptyTuple

  trait M2[A,B]
  type M2IM2SM2IM2IM2D = M2[Int, Unit] *: M2[String, Unit] *: M2[Int, Unit] *: M2[Int, Unit] *: M2[Double, Unit] *: EmptyTuple

  val m2i: M2[Int, Unit] = new M2[Int, Unit] {}
  val m2s: M2[String, Unit] = new M2[String, Unit] {}
  val m2d: M2[Double, Unit] = new M2[Double, Unit] {}
  val m2im2sm2im2im2dList = List(m2i, m2s, m2i, m2i, m2d)
  val m2im2sm2im2im2dArray = Array(m2i, m2s, m2i, m2i, m2d)
  val m2im2sm2im2im2d: M2IM2SM2IM2IM2D = m2i *: m2s *: m2i *: m2i *: m2d *: EmptyTuple

  val m2iExist: M2[Int, _] = new M2[Int, Unit] {}
  val m2sExist: M2[String, _] = new M2[String, Unit] {}
  val m2dExist: M2[Double, _] = new M2[Double, Unit] {}
  type M2EIM2ESM2EIM2EEM2ED = M2[Int, _] *: M2[String, _] *: M2[Int, _] *: M2[Int, _] *: M2[Double, _] *: EmptyTuple
  val m2eim2esm2eim2eem2edList = List(m2iExist, m2sExist, m2iExist, m2iExist, m2dExist)
  val m2eim2esm2eim2eem2edArray = Array(m2iExist, m2sExist, m2iExist, m2iExist, m2dExist)
  val m2eim2esm2eim2eem2ed: M2EIM2ESM2EIM2EEM2ED = m2iExist *: m2sExist *: m2iExist *: m2iExist *: m2dExist *: EmptyTuple

  object mkString extends (Any -> String)(_.toString)
  object fruit extends (Fruit -> Fruit)(f => f)
  object incInt extends (Int >-> Int)(_ + 1)
  // object extendedChoose extends LiftU(choose)

  @Test
  def testBasics: Unit = {
    val l = 1 *: "foo" *: 2.0 *: EmptyTuple

    val r1 = l.head
    assertTypedEquals[Int](1, r1)

    val r2 = l.tail.head
    assertTypedEquals[String]("foo", r2)

    assertEquals(2.0, l.tail.tail.head, Double.MinPositiveValue)

    illTyped("""
      EmptyTuple.head
    """)

    illTyped("""
      EmptyTuple.tail
    """)

    illTyped("""
      l.tail.tail.tail.head
    """)
  }

  @Test
  def testMap: Unit = {
    implicitly[Mapper.Aux[choose.type, EmptyTuple, EmptyTuple]]
    implicitly[choose.Case[Set[Int]]]
    implicitly[Mapper.Aux[choose.type, Set[Int] *: EmptyTuple, Option[Int] *: EmptyTuple]]

    val s1 = Set(1) *: EmptyTuple
    val o1 = s1 mapPoly choose
    assertTypedEquals[OI](Option(1) *: EmptyTuple, o1)

    val s2 = Set(1) *: Set("foo") *: EmptyTuple
    val o2 = s2 mapPoly choose
    assertTypedEquals[OIOS](Option(1) *: Option("foo") *: EmptyTuple, o2)

    val l1 = 1 *: "foo" *: 2 *: 3 *: EmptyTuple

    val l2 = l1 mapPoly singleton
    assertTypedEquals[SISSSISI](Set(1) *: Set("foo") *: Set(2) *: Set(3) *: EmptyTuple, l2)

    val l3 = l1 mapPoly option
    assertTypedEquals[OIOSOIOI](Option(1) *: Option("foo") *: Option(2) *: Option(3) *: EmptyTuple, l3)

    val l4 = Option(1) *: Option("foo") *: Option(2) *: Option(3) *: EmptyTuple

    val l5 = l4 mapPoly get
    assertTypedEquals[ISII](1 *: "foo" *: 2 *: 3 *: EmptyTuple, l5)

    typed[Int](l5.head)
    typed[String](l5.tail.head)
    typed[Int](l5.tail.tail.head)
    typed[Int](l5.tail.tail.tail.head)

    val l6 = l1 mapPoly id
    assertTypedEquals[ISII](1 *: "foo" *: 2 *: 3 *: EmptyTuple, l6)

    val l7 = l4 mapPoly isDefined
    assertTypedEquals[BBBB](true *: true *: true *: true *: EmptyTuple, l7)

    val l8 = 23 *: "foo" *: true *: EmptyTuple
    val l9 = l8 mapPoly mkString
    assertTypedEquals[String *: String *: String *: EmptyTuple]("23" *: "foo" *: "true" *: EmptyTuple, l9)

    val l10 = apbp mapPoly fruit
    assertTypedEquals[Fruit *: Fruit *: Fruit *: Fruit *: EmptyTuple](apbp, l10)

    val l11 = apbp mapPoly mkString
    assertTypedEquals[String *: String *: String *: String *: EmptyTuple]("Apple()" *: "Pear()" *: "Banana()" *: "Pear()" *: EmptyTuple, l11)
  }

  @Test
  def testMapped: Unit = {
    val meOption = Mapped[EmptyTuple, Option]
    val _ = summon[meOption.Out =:= EmptyTuple]

    val misOption = Mapped[Int *: String *: EmptyTuple, Option]
    val _ = summon[misOption.Out =:= Option[Int] *: Option[String] *: EmptyTuple]

    val meId = Mapped[EmptyTuple, [a] =>> a]
    val _ = summon[meId.Out =:= EmptyTuple]

    val misId = Mapped[Int *: String *: EmptyTuple, [a] =>> a]
    val _ = summon[misId.Out =:= Int *: String *: EmptyTuple]

    val meConstInt = Mapped[EmptyTuple, [a] =>> Int]
    val _ = summon[meConstInt.Out =:= EmptyTuple]

    val mdsConstInt = Mapped[Double *: String *: EmptyTuple, [a] =>> Int]
    val _ = summon[mdsConstInt.Out =:= Int *: Int *: EmptyTuple]
  }

  object dup extends Poly1 {
    given default[T]: Case.Aux[T, T *: T *: EmptyTuple] = at[T](t => t *: t *: EmptyTuple)
  }

  @Test
  def testFlatMap: Unit = {
    val l1 = 1 *: "foo" *: true *: EmptyTuple

    val l2 = l1 flatMap dup
    assertTypedEquals[Int *: Int *: String *: String *: Boolean *: Boolean *: EmptyTuple](
      1 *: 1 *: "foo" *: "foo" *: true *: true *: EmptyTuple, l2)

    val l3 = (1 *: "foo" *: EmptyTuple) *: (EmptyTuple: EmptyTuple) *: (2.0 *: true *: EmptyTuple) *: ("bar" *: EmptyTuple) *: EmptyTuple

    val l4 = l3 flatMap id
    assertTypedEquals[Int *: String *: Double *: Boolean *: String *: EmptyTuple](
      1 *: "foo" *: 2.0 *: true *: "bar" *: EmptyTuple, l4)

    val l5 = 23 *: "foo" *: 7 *: true *: 0 *: EmptyTuple
    val l6 = l5 flatMap incInt
    assertTypedEquals[Int *: Int *: Int *: EmptyTuple](24 *: 8 *: 1 *: EmptyTuple, l6)

    // val l7 = Set(23) *: "foo" *: Set(true) *: 23 *: EmptyTuple
    // val l8 = l7 flatMap extendedChoose
    // assertTypedEquals[Option[Int] *: Option[Boolean] *: EmptyTuple](Option(23) *: Option(true) *: EmptyTuple, l8)
  }

  @Test
  def testConformance: Unit = {
    val l1 = 1 *: "foo" *: 2 *: 3 *: EmptyTuple
    assertTypedEquals[Any *: AnyRef *: Any *: Any *: EmptyTuple](1 *: "foo" *: 2 *: 3 *: EmptyTuple, l1)

    val ap = a *: p *: EmptyTuple
    typed[AP](ap)
    val bp = b *: p *: EmptyTuple
    typed[BP](bp)
    val apap = a *: p *: a *: p *: EmptyTuple
    typed[APAP](apap)
    val apbp = a *: p *: b *: p *: EmptyTuple
    typed[APBP](apbp)
    val ffff: FFFF = apap
    typed[FFFF](ffff)
  }

  @Test
  def testLength: Unit = {
    val l0 = EmptyTuple
    typed[0](l0.length)
    assertEquals(0, l0.length)

    val l1 = 1 *: "foo" *: 2 *: 3 *: EmptyTuple
    typed[4](l1.length)
    assertEquals(4, l1.length)

    val ap = a *: p *: EmptyTuple
    typed[2](ap.length)
    assertEquals(2, ap.length)

    val bp = b *: p *: EmptyTuple
    typed[2](bp.length)
    assertEquals(2, bp.length)

    val apap = a *: p *: a *: p *: EmptyTuple
    typed[4](apap.length)
    assertEquals(4, apap.length)

    val apbp = a *: p *: b *: p *: EmptyTuple
    typed[4](apbp.length)
    assertEquals(4, apbp.length)

    val ffff: FFFF = apap
    typed[4](ffff.length)
    assertEquals(4, ffff.length)
  }

  @Test
  def testRuntimeLength: Unit = {
    assertEquals(0, EmptyTuple.runtimeLength)
    assertEquals(1, (123 *: EmptyTuple).runtimeLength)
    assertEquals(2, ("abc" *: 123 *: EmptyTuple).runtimeLength)
  }

  @Test
  def testRuntimeList: Unit = {
    assertEquals(Nil, EmptyTuple.runtimeList)
    assertEquals(List(123), (123 *: EmptyTuple).runtimeList)
    assertEquals(List("abc", 123), ("abc" *: 123 *: EmptyTuple).runtimeList)
  }

  @Test
  def testInitLast: Unit = {

    val lp = apbp.last
    assertTypedEquals[Pear](p, lp)

    val iapb = apbp.init
    assertTypedEquals[APB](a *: p *: b *: EmptyTuple, iapb)
  }

  @Test
  def testAlign: Unit = {
    type M0 = Int *: String *: Boolean *: EmptyTuple
    type M1 = Int *: Boolean *: String *: EmptyTuple
    type M2 = String *: Int *: Boolean *: EmptyTuple
    type M3 = String *: Boolean *: Int *: EmptyTuple
    type M4 = Boolean *: Int *: String *: EmptyTuple
    type M5 = Boolean *: String *: Int *: EmptyTuple

    val m0 = 13 *: "bar" *: false *: EmptyTuple
    val m1 = 13 *: false *: "bar" *: EmptyTuple
    val m2 = "bar" *: 13 *: false *: EmptyTuple
    val m3 = "bar" *: false *: 13 *: EmptyTuple
    val m4 = false *: 13 *: "bar" *: EmptyTuple
    val m5 = false *: "bar" *: 13 *: EmptyTuple

    val l = 23 *: "foo" *: true *: EmptyTuple

    val a0 = l.align(m0)
    assertTypedEquals[M0](23 *: "foo" *: true *: EmptyTuple, a0)

    val a1 = l.align(m1)
    assertTypedEquals[M1](23 *: true *: "foo" *: EmptyTuple, a1)

    val a2 = l.align(m2)
    assertTypedEquals[M2]("foo" *: 23 *: true *: EmptyTuple, a2)

    val a3 = l.align(m3)
    assertTypedEquals[M3]("foo" *: true *: 23 *: EmptyTuple, a3)

    val a4 = l.align(m4)
    assertTypedEquals[M4](true *: 23 *: "foo" *: EmptyTuple, a4)

    val a5 = l.align(m5)
    assertTypedEquals[M5](true *: "foo" *: 23 *: EmptyTuple, a5)

    val b0 = l.align[M0]
    assertTypedEquals[M0](23 *: "foo" *: true *: EmptyTuple, b0)

    val b1 = l.align[M1]
    assertTypedEquals[M1](23 *: true *: "foo" *: EmptyTuple, b1)

    val b2 = l.align[M2]
    assertTypedEquals[M2]("foo" *: 23 *: true *: EmptyTuple, b2)

    val b3 = l.align[M3]
    assertTypedEquals[M3]("foo" *: true *: 23 *: EmptyTuple, b3)

    val b4 = l.align[M4]
    assertTypedEquals[M4](true *: 23 *: "foo" *: EmptyTuple, b4)

    val b5 = l.align[M5]
    assertTypedEquals[M5](true *: "foo" *: 23 *: EmptyTuple, b5)

    val c0 = (EmptyTuple: EmptyTuple).align[EmptyTuple]
    typed[EmptyTuple](c0)

    val c1 = (23 *: EmptyTuple).align[Int *: EmptyTuple]
    typed[Int *: EmptyTuple](c1)

    val c2 = (23 *: "foo" *: EmptyTuple).align[String *: Int *: EmptyTuple]
    typed[String *: Int *: EmptyTuple](c2)

    illTyped("""
      (EmptyTuple: EmptyTuple).align[Int *: EmptyTuple]
    """)

    illTyped("""
      (23 *: EmptyTuple).align[String *: EmptyTuple]
    """)

    illTyped("""
      (23 *: "foo" *: EmptyTuple).align[String *: String *: EmptyTuple]
    """)
  }

  @Test
  def testReverse: Unit = {
    val pbpa = apbp.reverse
    assertTypedEquals[PBPA](p *: b *: p *: a *: EmptyTuple, pbpa)

    val al = a *: EmptyTuple
    val ral = al.reverse
    assertTypedEquals[Apple *: EmptyTuple](a *: EmptyTuple, ral)
  }

  @Test
  def testPrepend: Unit = {
    val apbp2 = ap ::: bp
    assertTypedEquals[APBP](a *: p *: b *: p *: EmptyTuple, apbp2)

    typed[Apple](apbp2.head)
    typed[Pear](apbp2.tail.head)
    typed[Banana](apbp2.tail.tail.head)
    typed[Pear](apbp2.tail.tail.tail.head)

    val pabp = ap reverse_::: bp
    assertTypedEquals[PABP](p *: a *: b *: p *: EmptyTuple, pabp)

    {
      // must compile without requiring an implicit Prepend
      def prependWithEmptyTuple[L <: Tuple](list: L) = EmptyTuple ::: list
      def prependToEmptyTuple[L <: Tuple](list: L) = list ::: EmptyTuple

      val r1 = prependWithEmptyTuple(ap)
      assertTypedSame[AP](ap, r1)
      val r2 = prependToEmptyTuple(ap)
      assertTypedSame[AP](ap, r2)
      val r3 = EmptyTuple ::: EmptyTuple
      assertTypedSame[EmptyTuple](EmptyTuple, r3)

      val r4 = prependWithEmptyTuple(pabp)
      assertTypedSame[PABP](pabp, r4)
      val r5 = prependToEmptyTuple(pabp)
      assertTypedSame[PABP](pabp, r5)
    }

    {
      // must also pass with the default implicit
      val r1 = EmptyTuple ::: ap
      assertTypedSame[AP](ap, r1)
      val r2 = ap ::: EmptyTuple
      assertTypedSame[AP](ap, r2)

      val r4 = EmptyTuple ::: pabp
      assertTypedSame[PABP](pabp, r4)
      val r5 = pabp ::: EmptyTuple
      assertTypedSame[PABP](pabp, r5)
    }

    {
      // must compile without requiring an implicit ReversePrepend
      def reversePrependWithEmptyTuple[L <: Tuple](list: L) = EmptyTuple reverse_::: list
      def reversePrependToEmptyTuple[L <: Tuple: Reverse](list: L) = list reverse_::: EmptyTuple
      val r4 = reversePrependWithEmptyTuple(ap)
      assertTypedSame[AP](ap, r4)
      val r5 = reversePrependToEmptyTuple(ap)
      assertTypedEquals[Pear *: Apple *: EmptyTuple](ap.reverse, r5)
      val r6 = EmptyTuple reverse_::: EmptyTuple
      assertTypedSame[EmptyTuple](EmptyTuple, r6)
    }
  }

  @Test
  def testRepeat: Unit = {
    val ap2 = ap.repeat[2]
    assertTypedEquals[Apple *: Pear *: Apple *: Pear *: EmptyTuple](ap2, a *: p *: a *: p *: EmptyTuple)

    val ap4 = ap.repeat[4]
    assertTypedEquals[Apple *: Pear *: Apple *: Pear *: Apple *: Pear *: Apple *: Pear *: EmptyTuple](
      ap4, a *: p *: a *: p *: a *: p *: a *: p *: EmptyTuple
    )

    val ap2_2 = ap2.repeat[2]
    assertTypedEquals[Apple *: Pear *: Apple *: Pear *: Apple *: Pear *: Apple *: Pear *: EmptyTuple](ap2_2, ap4)

    {
      // repeating 1 times is identity
      val ap1 = ap.repeat[1]
      assertTypedEquals[AP](ap, ap1)
    }

    {
      // can not repeat zero times
      illTyped("""ap.repeat[0]""")
    }

  }

  // @Test
  // def testToSizedList: Unit = {
  //   def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B): Unit = {}

  //   val hnil = EmptyTuple
  //   val snil = hnil.toSized[List]
  //   assertEquals(hnil.length, snil.length)
  //   val expectedUnsized = List.empty[Nothing]
  //   equalInferredTypes(expectedUnsized, snil.unsized)
  //   assertEquals(expectedUnsized, snil.unsized)

  //   implicitly[ToSized.Aux[EmptyTuple, List, Nothing, _0]]
  //   implicitly[ToSized.Aux[EmptyTuple, List, Int, _0]]

  //   {
  //     implicitly[ToSized.Aux[M[Int] *: EmptyTuple, List, M[Int], _1]]
  //     implicitly[ToSized.Aux[M[Int] *: EmptyTuple, List, M[_], _1]]
  //   }

  //   val sizedApap = apap.toSized[List]
  //   assertEquals(apap.length, sizedApap.length)
  //   equalInferredTypes(apapList, sizedApap.unsized)
  //   assertEquals(apapList, sizedApap.unsized)

  //   val sizedApbp = apbp.toSized[List]
  //   assertEquals(apbp.length, sizedApbp.length)
  //   equalInferredTypes(apbpList, sizedApbp.unsized)
  //   assertEquals(apbpList, sizedApbp.unsized)

  //   val sizedCicscicicd = cicscicicd.toSized[List]
  //   assertEquals(cicscicicd.length, sizedCicscicicd.length)
  //   equalInferredTypes(cicscicicdList, sizedCicscicicd.unsized)
  //   assertEquals(cicscicicdList, sizedCicscicicd.unsized)

  //   val sizedMimsmimimd = mimsmimimd.toSized[List]
  //   assertEquals(mimsmimimd.length, sizedMimsmimimd.length)
  //   equalInferredTypes(mimsmimimdList, sizedMimsmimimd.unsized)
  //   assertEquals(mimsmimimdList, sizedMimsmimimd.unsized)

  //   val sizedMimsmimemd = mimsmimemd.toSized[List]
  //   assertEquals(mimsmimemd.length, sizedMimsmimemd.length)
  //   // equalInferredTypes(mimsmimemdList, sizedMimsmimemd.unsized)
  //   typed[List[M[_]]](sizedMimsmimemd.unsized)
  //   assertEquals(mimsmimemdList, sizedMimsmimemd.unsized)

  //   val sizedM2im2sm2im2im2d = m2im2sm2im2im2d.toSized[List]
  //   assertEquals(m2im2sm2im2im2d.length, sizedM2im2sm2im2im2d.length)
  //   equalInferredTypes(m2im2sm2im2im2dList, sizedM2im2sm2im2im2d.unsized)
  //   assertEquals(m2im2sm2im2im2dList, sizedM2im2sm2im2im2d.unsized)

  //   val sizedM2eim2esm2eim2eem2ed = m2eim2esm2eim2eem2ed.toSized[List]
  //   assertEquals(m2eim2esm2eim2eem2ed.length, sizedM2eim2esm2eim2eem2ed.length)
  //   // equalInferredTypes(m2eim2esm2eim2eem2edList, sizedM2eim2esm2eim2eem2ed.unsized)
  //   assertTypedEquals[List[M2[_ >: Double with Int with String, _]]](
  //     m2eim2esm2eim2eem2edList, sizedM2eim2esm2eim2eem2ed.unsized)
  // }

  // @Test
  // def testToSizedArray: Unit = {
  //   def assertArrayEquals2[T](arr1: Array[T], arr2: Array[T]) =
  //     assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr2.asInstanceOf[Array[Object]])

  //   def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B): Unit = {}

  //   val hnil = EmptyTuple
  //   val snil = hnil.toSized[Array]
  //   assertEquals(hnil.length, snil.length)
  //   val expectedUnsized = Array.empty[Nothing]
  //   equalInferredTypes(expectedUnsized, snil.unsized)
  //   assertArrayEquals2(expectedUnsized, snil.unsized)

  //   implicitly[ToSized.Aux[EmptyTuple, Array, Nothing, _0]]
  //   implicitly[ToSized.Aux[EmptyTuple, Array, Int, _0]]

  //   val sizedApap = apap.toSized[Array]
  //   assertEquals(apap.length, sizedApap.length)
  //   equalInferredTypes(apapArray, sizedApap.unsized)
  //   assertArrayEquals2(apapArray, sizedApap.unsized)

  //   val sizedApbp = apbp.toSized[Array]
  //   assertEquals(apbp.length, sizedApbp.length)
  //   equalInferredTypes(apbpArray, sizedApbp.unsized)
  //   assertArrayEquals2(apbpArray, sizedApbp.unsized)

  //   val sizedCicscicicd = cicscicicd.toSized[Array]
  //   assertEquals(cicscicicd.length, sizedCicscicicd.length)
  //   equalInferredTypes(cicscicicdArray, sizedCicscicicd.unsized)
  //   assertArrayEquals2(cicscicicdArray, sizedCicscicicd.unsized)

  //   val sizedMimsmimimd = mimsmimimd.toSized[Array]
  //   assertEquals(mimsmimimd.length, sizedMimsmimimd.length)
  //   equalInferredTypes(mimsmimimdArray, sizedMimsmimimd.unsized)
  //   assertArrayEquals2(mimsmimimdArray, sizedMimsmimimd.unsized)

  //   val sizedMimsmimemd = mimsmimemd.toSized[Array]
  //   assertEquals(mimsmimemd.length, sizedMimsmimemd.length)
  //   // equalInferredTypes(mimsmimemdArray, sizedMimsmimemd.unsized)
  //   typed[Array[M[_]]](sizedMimsmimemd.unsized)
  //   assertArrayEquals2(mimsmimemdArray, sizedMimsmimemd.unsized)

  //   val sizedM2im2sm2im2im2d = m2im2sm2im2im2d.toSized[Array]
  //   assertEquals(m2im2sm2im2im2d.length, sizedM2im2sm2im2im2d.length)
  //   equalInferredTypes(m2im2sm2im2im2dArray, sizedM2im2sm2im2im2d.unsized)
  //   assertArrayEquals2(m2im2sm2im2im2dArray, sizedM2im2sm2im2im2d.unsized)

  //   val sizedM2eim2esm2eim2eem2ed = m2eim2esm2eim2eem2ed.toSized[Array]
  //   assertEquals(m2eim2esm2eim2eem2ed.length, sizedM2eim2esm2eim2eem2ed.length)
  //   // equalInferredTypes(m2eim2esm2eim2eem2edArray, sizedM2eim2esm2eim2eem2ed.unsized)
  //   typed[Array[M2[_ >: Double with Int with String, _]]](sizedM2eim2esm2eim2eem2ed.unsized)
  //   assertArrayEquals2(m2eim2esm2eim2eem2edArray.map(x => x: Any), sizedM2eim2esm2eim2eem2ed.unsized.map(x => x: Any))
  // }

  @Test
  def testUnifier: Unit = {
    def lub[X, Y, L](x: X, y: Y)(implicit lb: Lub[X, Y, L]): (L, L) = (lb.left(x), lb.right(y))

    val u21 = lub(a, a)
    typed[(Apple, Apple)](u21)
    val u22 = lub(a, p)
    typed[(Fruit, Fruit)](u22)
    val u23 = lub(a, f)
    typed[(Fruit, Fruit)](u23)
    val u24 = lub(p, a)
    typed[(Fruit, Fruit)](u24)
    val u25 = lub(p, p)
    typed[(Pear, Pear)](u25)
    val u26 = lub(f, f)
    typed[(Fruit, Fruit)](u26)
    val u27 = lub(f, a)
    typed[(Fruit, Fruit)](u27)
    val u28 = lub(f, p)
    typed[(Fruit, Fruit)](u28)
    val u29 = lub(f, f)
    typed[(Fruit, Fruit)](u29)

    implicitly[Lub[EmptyTuple, EmptyTuple, EmptyTuple]]
    implicitly[Lub[Apple *: EmptyTuple, Apple *: EmptyTuple, Apple *: EmptyTuple]]
    implicitly[Lub[Fruit *: Pear *: EmptyTuple, Fruit *: Fruit *: EmptyTuple, Fruit *: Fruit *: EmptyTuple]]
    implicitly[Lub[Apple *: Pear *: EmptyTuple, Pear *: Apple *: EmptyTuple, Fruit *: Fruit *: EmptyTuple]]
    implicitly[Lub[ISII, IIII, IYII]]

    val u31 = lub(EmptyTuple, EmptyTuple)
    typed[(EmptyTuple, EmptyTuple)](u31)
    val u32 = lub(a *: EmptyTuple, a *: EmptyTuple)
    typed[(Apple *: EmptyTuple, Apple *: EmptyTuple)](u32)
    val u33 = lub(f *: p *: EmptyTuple, f *: f *: EmptyTuple)
    typed[(Fruit *: Fruit *: EmptyTuple, Fruit *: Fruit *: EmptyTuple)](u33)
    val u34 = lub(a *: p *: EmptyTuple, p *: a *: EmptyTuple)
    typed[(Fruit *: Fruit *: EmptyTuple, Fruit *: Fruit *: EmptyTuple)](u34)
    val u35 = lub(1 *: "two" *: 3 *: 4 *: EmptyTuple, 1 *: 2 *: 3 *: 4 *: EmptyTuple)
    typed[(Int *: Any *: Int *: Int *: EmptyTuple, Int *: Any *: Int *: Int *: EmptyTuple)](u35)

    implicitly[Unifier.Aux[Apple *: EmptyTuple, Apple *: EmptyTuple]]
    implicitly[Unifier.Aux[Fruit *: Pear *: EmptyTuple, Fruit *: Fruit *: EmptyTuple]]
    implicitly[Unifier.Aux[Apple *: Pear *: EmptyTuple, Fruit *: Fruit *: EmptyTuple]]

    implicitly[Unifier.Aux[Int *: String *: Int *: Int *: EmptyTuple, YYYY]]

    val uapap = implicitly[Unifier.Aux[Apple *: Pear *: Apple *: Pear *: EmptyTuple, FFFF]]
    val unified1 = uapap(apap)
    typed[FFFF](unified1)
    val unified2 = apap.unify
    typed[FFFF](unified2)

    val ununified1 = summon[TypeTest[unified2.type, APAP]].unapply(unified2)
    assertTrue(ununified1.isDefined)
    typed[APAP](ununified1.get)
    val ununified2 = summon[TypeTest[unified2.type, APBP]].unapply(unified2)
    assertFalse(ununified2.isDefined)
    typed[Option[APBP]](ununified2)

    def getUnifier[L <: Tuple, Out <: Tuple](l: L)(implicit u: Unifier.Aux[L, Out]) = u

    val u2 = getUnifier(a *: EmptyTuple)
    typed[Unifier.Aux[Apple *: EmptyTuple, Apple *: EmptyTuple]](u2)
    val u3 = getUnifier(a *: a *: EmptyTuple)
    typed[Unifier.Aux[Apple *: Apple *: EmptyTuple, Apple *: Apple *: EmptyTuple]](u3)
    val u4 = getUnifier(a *: a *: a *: EmptyTuple)
    typed[Unifier.Aux[Apple *: Apple *: Apple *: EmptyTuple, Apple *: Apple *: Apple *: EmptyTuple]](u4)
    val u5 = getUnifier(a *: a *: a *: a *: EmptyTuple)
    typed[Unifier.Aux[Apple *: Apple *: Apple *: Apple *: EmptyTuple, Apple *: Apple *: Apple *: Apple *: EmptyTuple]](u5)
    val u6 = getUnifier(a *: p *: EmptyTuple)
    //typed[Unifier.Aux[Apple *: Pear *: EmptyTuple, Fruit *: Fruit *: EmptyTuple]](u6)
    val u7 = getUnifier(a *: f *: EmptyTuple)
    typed[Unifier.Aux[Apple *: Fruit *: EmptyTuple, Fruit *: Fruit *: EmptyTuple]](u7)
    val u8 = getUnifier(f *: a *: EmptyTuple)
    typed[Unifier.Aux[Fruit *: Apple *: EmptyTuple, Fruit *: Fruit *: EmptyTuple]](u8)
    val u9a = getUnifier(a *: f *: EmptyTuple)
    typed[Unifier.Aux[Apple *: Fruit *: EmptyTuple, FF]](u9a)
    val u9b = getUnifier(a *: p *: EmptyTuple)
    typed[Unifier.Aux[Apple *: Pear *: EmptyTuple, Fruit *: Fruit *: EmptyTuple]](u9b)
    val u10 = getUnifier(apap)
    typed[Unifier.Aux[APAP, Fruit *: Fruit *: Fruit *: Fruit *: EmptyTuple]](u10)
    val u11 = getUnifier(apbp)
    typed[Unifier.Aux[APBP, Fruit *: Fruit *: Fruit *: Fruit *: EmptyTuple]](u11)

    val invar1 = Set(23) *: Set("foo") *: EmptyTuple
    val uinvar1 = invar1.unify
    typed[Set[_ >: Int with String] *: Set[_ >: Int with String] *: EmptyTuple](uinvar1)

    // Unifying three or more elements which have an invariant outer type constructor and differing type
    // arguments fails, presumably due to a failure to compute a sensible LUB.
    //val invar2 = Set(23) *: Set("foo") *: Set(true) *: EmptyTuple
    //val uinvar2 = invar.unify
  }

  @Test
  def testSubtypeUnifier: Unit = {
    val fruits: Apple *: Pear *: Fruit *: EmptyTuple = a *: p *: f *: EmptyTuple
    typed[Fruit *: Fruit *: Fruit *: EmptyTuple](fruits.unifySubtypes[Fruit])
    typed[Apple *: Pear *: Fruit *: EmptyTuple](fruits.unifySubtypes[Apple])
    assertEquals(a *: p *: f *: EmptyTuple, fruits.unifySubtypes[Fruit].filter[Fruit])

    val stuff: Apple *: String *: Pear *: EmptyTuple = a *: "foo" *: p *: EmptyTuple
    typed[Fruit *: String *: Fruit *: EmptyTuple](stuff.unifySubtypes[Fruit])
    assertEquals(EmptyTuple, stuff.filter[Fruit])
    assertEquals(a *: p *: EmptyTuple, stuff.unifySubtypes[Fruit].filter[Fruit])
  }

  @Test
  def testToTraversableList: Unit = {
    val r1 = EmptyTuple.to[List]
    assertTypedEquals[List[Nothing]](Nil, r1)

    ToList[EmptyTuple, Nothing]
    ToList[EmptyTuple, Int]

    {
      implicitly[ToTraversable.Aux[M[Int] *: EmptyTuple, List, M[Int]]]
      implicitly[ToTraversable.Aux[M[Int] *: EmptyTuple, List, M[_]]]
    }

    val r2 = apap.to[List]
    assertTypedEquals[List[Fruit]](List(a, p, a, p), r2)

    val fruits2 = apbp.to[List]
    assertTypedEquals[List[Fruit]](List(a, p, b, p), fruits2)

    // val fruits3 = fruits2.toTuple[APBP]
    // assertTrue(fruits3.isDefined)
    // assertTypedEquals[APBP](apbp, fruits3.get)

    val stuff = (1 *: "foo" *: 2 *: 3 *: EmptyTuple).to[List]
    assertTypedEquals[List[Any]](List(1, "foo", 2, 3), stuff)

    // val stuff2 = stuff.toTuple[ISII]
    // assertTrue(stuff2.isDefined)
    // assertTypedEquals[ISII](1 *: "foo" *: 2 *: 3 *: EmptyTuple, stuff2.get)

    val l4 = Option(1) *: Option("foo") *: Option(2) *: Option(3) *: EmptyTuple
    val l7 = l4 mapPoly isDefined
    assertTypedEquals[BBBB](true *: true *: true *: true *: EmptyTuple, l7)

    val ll2 = l7.to[List]
    typed[Boolean](ll2.head)

    val moreStuff = (a *: "foo" *: p *: EmptyTuple).to[List]
    typed[List[Any]](moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B): Unit = {}

    val ctv = cicscicicd.to[List]
    equalInferredTypes(cicscicicdList, ctv)
    assertTypedEquals[List[Ctv[Int with String with Double]]](cicscicicdList, ctv)

    val m = mimsmimimd.to[List]
    equalInferredTypes(mimsmimimdList, m)
    assertTypedEquals[List[M[_ >: Int with String with Double]]](mimsmimimdList, m)

    val mWithEx = mimsmimemd.to[List]
    //  equalType(mimsmimemdList, mWithEx)
    assertTypedEquals[List[M[_]]](mimsmimemdList, mWithEx)

    val m2 = m2im2sm2im2im2d.to[List]
    equalInferredTypes(m2im2sm2im2im2dList, m2)
    assertTypedEquals[List[M2[_ >: Int with String with Double, Unit]]](m2im2sm2im2im2dList, m2)

    val m2e = m2eim2esm2eim2eem2ed.to[List]
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    assertTypedEquals[List[M2[_ >: Int with String with Double, _]]](m2eim2esm2eim2eem2edList, m2e)
  }

  // @Test
  // def testToPreciseList: Unit = {
  //   val r1 = EmptyTuple.toCoproduct[List]
  //   assertTypedEquals[List[CNil]](Nil, r1)

  //   val r2 = ap.toCoproduct[List]
  //   assertTypedEquals[List[APc]](List(Coproduct[APc](a), Coproduct[APc](p)), r2)

  //   val r3 = apap.toCoproduct[List]
  //   assertTypedEquals[List[APc]](List(Coproduct[APc](a), Coproduct[APc](p), Coproduct[APc](a), Coproduct[APc](p)), r3)

  //   val r4 = apbp.toCoproduct[Vector]
  //   assertTypedEquals[Vector[ABPc]](Vector[ABPc](Coproduct[ABPc](a), Coproduct[ABPc](p), Coproduct[ABPc](b), Coproduct[ABPc](p)), r4)

  //   def equalInferedCoproducts[A <: Coproduct, B <: Coproduct](a: A, b: B)(implicit bInA: ops.coproduct.Basis[A, B], aInB: ops.coproduct.Basis[B, A]): Unit = {}
  //   val abpc = Coproduct[ABPc](a)

  //   val r5 = (a *: b *: a *: p *: b *: a *: EmptyTuple).toCoproduct[Set]
  //   equalInferedCoproducts(abpc, r5.head)

  //   val r6 = (p *: a *: a *: p *: p *: b *: EmptyTuple).toCoproduct[Set]
  //   equalInferedCoproducts(abpc, r6.head)

  //   val r7 = (a *: b *: p *: EmptyTuple).toCoproduct[Seq]
  //   equalInferedCoproducts(abpc, r7.head)


  //   val r8 = (a *: b *: EmptyTuple).toCoproduct[Seq]

  //   illTyped{
  //     """equalInferedCoproducts(abpc, r8.head)"""
  //   }

  //   illTyped{
  //     """(1 *: "foo" *: EmptyTuple).toPrecise[Array]"""
  //   }

  // }

  @Test
  def testToList: Unit = {
    val r1 = EmptyTuple.toListLub
    assertTypedEquals[List[Nothing]](Nil, r1)

    implicitly[ToTraversable.Aux[EmptyTuple, List, Nothing]]
    implicitly[ToTraversable.Aux[EmptyTuple, List, Int]]

    {
      val l1 = (mi *: EmptyTuple).toListLub[M[Int]]
      val l2 = (mi *: EmptyTuple).toListLub[M[_]]

      assertTypedEquals[List[M[Int]]](List(mi), l1)
      assertTypedEquals[List[M[_]]](List(mi), l2)
    }

    val fruits1 = apap.toListLub
    assertTypedEquals[List[Fruit]](List(a, p, a, p), fruits1)

    val fruits2 = apbp.toListLub
    assertTypedEquals[List[Fruit]](List(a, p, b, p), fruits2)

    // val fruits3 = fruits2.toTuple[APBP]
    // assertTrue(fruits3.isDefined)
    // assertTypedEquals[APBP](apbp, fruits3.get)

    val l1 = 1 *: "foo" *: 2 *: 3 *: EmptyTuple

    val stuff = l1.toListLub
    assertTypedEquals[List[Any]](List(1, "foo", 2, 3), stuff)

    // val stuff2 = stuff.toTuple[ISII]
    // assertTrue(stuff2.isDefined)
    // assertTypedEquals[ISII](1 *: "foo" *: 2 *: 3 *: EmptyTuple, stuff2.get)

    val l4 = Option(1) *: Option("foo") *: Option(2) *: Option(3) *: EmptyTuple
    val l7 = l4 mapPoly isDefined
    assertTypedEquals[BBBB](true *: true *: true *: true *: EmptyTuple, l7)

    val ll2 = l7.toListLub
    typed[Boolean](ll2.head)

    val moreStuff = (a *: "foo" *: p *: EmptyTuple).toListLub
    typed[List[Any]](moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B): Unit = {}

    val ctv = cicscicicd.toListLub
    equalInferredTypes(cicscicicdList, ctv)
    assertTypedEquals[List[Ctv[Int with String with Double]]](cicscicicdList, ctv)

    val m = mimsmimimd.toListLub
    equalInferredTypes(mimsmimimdList, m)
    assertTypedEquals[List[M[_ >: Int with String with Double]]](mimsmimimdList, m)

    // With existentials, it gets more tricky
    val mWithEx = mimsmimemd.toListLub
    // Compiler fails complaining that it
    //    Cannot prove that List[TupleTests.this.M[_ >: Double with _$1 with Int with String]] =:= List[TupleTests.this.M[_]]
    //  equalType(mimsmimemdList, mWithEx)
    assertTypedEquals[List[M[_]]](mimsmimemdList, mWithEx)

    // Second order higher kinded types are ok...
    val m2 = m2im2sm2im2im2d.toListLub
    equalInferredTypes(m2im2sm2im2im2dList, m2)
    assertTypedEquals[List[M2[_ >: Int with String with Double, Unit]]](m2im2sm2im2im2dList, m2)

    // ...as long as existentials are not involved.
    val m2e = m2eim2esm2eim2eem2ed.toListLub
    // Compiler complains that it
    //    Cannot prove that List[TupleTests.this.M2[_ >: Double with Int with Int with String with Int, _ >: _$5 with _$3 with _$3 with _$4 with _$3]] =:= List[TupleTests.this.M2[35,36] forSome { type _$10; type _$9; type 34 >: _$10 with _$9; type _$8; type _$7; type 32 >: _$8 with _$7; type 35 >: Double with Int with Int with String; type 36 >: _34 with 32 }]
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    assertTypedEquals[List[M2[_ >: Int with String with Double, _]]](m2eim2esm2eim2eem2edList, m2e)
  }

  @Test
  def testToTraversableArray: Unit = {
    def assertArrayEquals2[T](arr1: Array[T], arr2: Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr2.asInstanceOf[Array[Object]])

    val empty: Array[Unit] = EmptyTuple.to[Array]
    typed[Array[Unit]](empty)
    assertArrayEquals2(Array[Unit](), empty)

    implicitly[ToTraversable.Aux[EmptyTuple, Array, Unit]]
    implicitly[ToTraversable.Aux[EmptyTuple, Array, Int]]

    {
      implicitly[ToTraversable.Aux[M[Int] *: EmptyTuple, Array, M[Int]]]
      implicitly[ToTraversable.Aux[M[Int] *: EmptyTuple, Array, M[_]]]
    }

    val fruits1 = apap.to[Array].map(x => x: Fruit) // Default inferred type is too precise
                                                     // (Product with Serializable with Fruit)
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)

    val fruits2 = apbp.to[Array].map(x => x: Fruit)
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)

    // val fruits3 = fruits2.toTuple[APBP]
    // assertTrue(fruits3.isDefined)
    // assertTypedEquals[APBP](apbp, fruits3.get)

    val l1 = 1 *: "foo" *: 2 *: 3 *: EmptyTuple

    val stuff = l1.to[Array]
    typed[Array[Int | String]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)

    // val stuff2 = stuff.toTuple[ISII]
    // assertTrue(stuff2.isDefined)
    // assertTypedEquals[ISII](1 *: "foo" *: 2 *: 3 *: EmptyTuple, stuff2.get)

    val l4 = Option(1) *: Option("foo") *: Option(2) *: Option(3) *: EmptyTuple
    val l7 = l4 mapPoly isDefined
    assertTypedEquals[BBBB](true *: true *: true *: true *: EmptyTuple, l7)

    val ll2: Array[Boolean] = l7.to[Array]
    typed[Boolean](ll2(0))

    val moreStuff = (a *: "foo" *: p *: EmptyTuple).to[Array].map(x => x: AnyRef)
    typed[Array[AnyRef]](moreStuff)
    assertArrayEquals2(Array[AnyRef](a, "foo", p), moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B): Unit = {}

    val ctv = cicscicicd.to[Array]
    equalInferredTypes(cicscicicdArray, ctv)
    typed[Array[Ctv[Int with String with Double]]](ctv)
    assertArrayEquals2(cicscicicdArray, ctv)

    val m = mimsmimimd.to[Array]
    equalInferredTypes(mimsmimimdArray, m)
    typed[Array[M[_ >: String & Int & Double <: String | Int | Double]]](m)
    assertArrayEquals2(mimsmimimdArray, m)

    val mWithEx = mimsmimemd.to[Array]
    //  equalType(mimsmimemdArray, mWithEx)
    typed[Array[M[_]]](mWithEx)
    assertArrayEquals2(mimsmimemdArray, mWithEx)

    val m2 = m2im2sm2im2im2d.to[Array]
    equalInferredTypes(m2im2sm2im2im2dArray, m2)
    typed[Array[M2[_ >: String & Int & Double <: String | Int | Double, Unit]]](m2)
    assertArrayEquals2(m2im2sm2im2im2dArray, m2)

    val m2e = m2eim2esm2eim2eem2ed.to[Array]
    // equalInferredTypes(m2eim2esm2eim2eem2edArray, m2e)
    typed[Array[M2[_ >: String & Int & Double <: String | Int | Double, _]]](m2e)
    assertArrayEquals2(m2eim2esm2eim2eem2edArray.map(x => x: Any), m2e.map(x => x: Any))
  }

  @Test
  def testToArray: Unit = {
    def assertArrayEquals2[T](arr1: Array[T], arr2: Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr2.asInstanceOf[Array[Object]])

    val empty: Array[Unit] = EmptyTuple.toArrayLub
    typed[Array[Unit]](empty)
    assertArrayEquals2(Array[Unit](), empty)

    ToArray[EmptyTuple, Unit]
    ToArray[EmptyTuple, Int]

    {
      val a1 = (mi *: EmptyTuple).toArrayLub[M[Int]]
      val a2 = (mi *: EmptyTuple).toArrayLub[M[_]]

      typed[Array[M[Int]]](a1)
      typed[Array[M[_]]](a2)
      assertArrayEquals2(Array[M[Int]](mi), a1)
      assertArrayEquals2(Array[M[_]](mi), a2)
    }

    val fruits1 = apap.toArrayLub[Fruit]
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)

    val fruits2 = apbp.toArrayLub[Fruit]
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)

    // val fruits3 = fruits2.toTuple[APBP]
    // assertTrue(fruits3.isDefined)
    // assertTypedEquals[APBP](apbp, fruits3.get)

    val l1 = 1 *: "foo" *: 2 *: 3 *: EmptyTuple

    val stuff = l1.toArrayLub
    typed[Array[Int | String]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)

    val ssl = "foo" *: "bar" *: 1L *: EmptyTuple
    val ssla = ssl.toArrayLub
    typed[Array[String | Long]](ssla)
    assertArrayEquals2(Array("foo", "bar", 1L), ssla)

    // val stuff2 = stuff.toTuple[ISII]
    // assertTrue(stuff2.isDefined)
    // assertTypedEquals[ISII](1 *: "foo" *: 2 *: 3 *: EmptyTuple, stuff2.get)

    val l4 = Option(1) *: Option("foo") *: Option(2) *: Option(3) *: EmptyTuple
    val l7 = l4 mapPoly isDefined
    assertTypedEquals[BBBB](true *: true *: true *: true *: EmptyTuple, l7)

    val ll2 = l7.toArrayLub[Boolean]
    typed[Boolean](ll2(0))

    val moreStuff = (a *: "foo" *: p *: EmptyTuple).toArrayLub[AnyRef]
    typed[Array[AnyRef]](moreStuff)
    assertArrayEquals2(Array[AnyRef](a, "foo", p), moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B): Unit = {}

    val ctv = cicscicicd.toArrayLub
    equalInferredTypes(cicscicicdArray, ctv)
    typed[Array[Ctv[Int & String & Double]]](ctv)
    assertArrayEquals2(cicscicicdArray, ctv)

    val m = mimsmimimd.toArrayLub
    equalInferredTypes(mimsmimimdArray, m)
    typed[Array[M[_ >: String & Int & Double <: String | Int | Double]]](m)
    assertArrayEquals2(mimsmimimdArray, m)

    val mWithEx = mimsmimemd.toArrayLub[M[_]]
    //  equalType(mimsmimemdArray, mWithEx)
    typed[Array[M[_]]](mWithEx)
    assertArrayEquals2(mimsmimemdArray, mWithEx)

    val m2 = m2im2sm2im2im2d.toArrayLub
    equalInferredTypes(m2im2sm2im2im2dArray, m2)
    typed[Array[M2[_ >: String & Int & Double <: String | Int | Double, Unit]]](m2)
    assertArrayEquals2(m2im2sm2im2im2dArray, m2)

    val m2e = m2eim2esm2eim2eem2ed.toArrayLub
    // equalInferredTypes(m2eim2esm2eim2eem2edArray, m2e)
    typed[Array[M2[_ >: String & Int & Double <: String | Int | Double, _]]](m2e)
    assertArrayEquals2(m2eim2esm2eim2eem2edArray.map(x => x: Any), m2e.map(x => x: Any))
  }

  @Test
  def testFoldMap: Unit = {
    implicitly[Mapper.Aux[isDefined.type, EmptyTuple, EmptyTuple]]
    implicitly[Mapper.Aux[isDefined.type, Option[Int] *: EmptyTuple, Boolean *: EmptyTuple]]

    val tl1 = Option(1) *: Option("foo") *: Option(2) *: Option(3) *: EmptyTuple
    val tl2 = Option(1) *: Option("foo") *: (None: Option[Int]) *: Option(3) *: EmptyTuple

    val mlfl1 = (tl1 mapPoly isDefined).toListLub.foldLeft(true)(_ && _)
    assertTrue(mlfl1)
    val mlfl2 = (tl2 mapPoly isDefined).toListLub.foldLeft(true)(_ && _)
    assertFalse(mlfl2)

    val fl1 = tl1.foldMap(true)(isDefined)(_ && _)
    assertTrue(fl1)
    val fl2 = tl2.foldMap(true)(isDefined)(_ && _)
    assertFalse(fl2)
  }

  @Test
  def testAt: Unit = {
    val sn1 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val at0 = sn1(0)
    assertTypedEquals[Int](23, at0)

    val at1 = sn1(1)
    typed[Double](at1)
    assertEquals(3.0, at1, Double.MinPositiveValue)

    val at2 = sn1(2)
    assertTypedEquals[String]("foo", at2)

    val at3 = sn1(3)
    assertTypedEquals[Unit]((), at3)

    val at4 = sn1(4)
    assertTypedEquals[String]("bar", at4)

    val at5 = sn1(5)
    assertTypedEquals[Boolean](true, at5)

    val at6 = sn1(6)
    assertTypedEquals[Long](5L, at6)

    val sn2 =
      0 *: 1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *:
      10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *:
      20 *: 21 *: 22 *: EmptyTuple

    val at22 = sn2(22)
    assertTypedEquals[Int](22, at22)
  }

  @Test
  def testAtLiteral: Unit = {
    val sn1 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val at0 = sn1(0)
    assertTypedEquals[Int](23, at0)

    val at1 = sn1(1)
    typed[Double](at1)
    assertEquals(3.0, at1, Double.MinPositiveValue)

    val at2 = sn1(2)
    assertTypedEquals[String]("foo", at2)

    val at3 = sn1(3)
    assertTypedEquals[Unit]((), at3)

    val at4 = sn1(4)
    assertTypedEquals[String]("bar", at4)

    val at5 = sn1(5)
    assertTypedEquals[Boolean](true, at5)

    val at6 = sn1(6)
    assertTypedEquals[Long](5L, at6)

    val sn2 =
      0 *: 1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *:
      10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *:
      20 *: 21 *: 22 *: EmptyTuple

    val at22 = sn2(22)
    assertTypedEquals[Int](22, at22)
  }

  @Test
  def testTakeDrop: Unit = {
    val sn1 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val r1 = sn1.take(0)
    assertTypedEquals[EmptyTuple](EmptyTuple, r1)

    val r2 = sn1.drop(0)
    assertTypedEquals[Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple](
      23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple, r2)

    val r3 = sn1.take(2)
    assertTypedEquals[Int *: Double *: EmptyTuple](23 *: 3.0 *: EmptyTuple, r3)

    val r4 = sn1.drop(2)
    assertTypedEquals[String *: Unit *: String *: Boolean *: Long *: EmptyTuple](
      "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple, r4)

    val r5 = sn1.take(7)
    assertTypedEquals[Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple](
      23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple, r5)

    val r6 = sn1.drop(7)
    assertTypedEquals[EmptyTuple](EmptyTuple, r6)
  }

  @Test
  def testTakeDropLiteral: Unit = {
    val sn1 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val r1 = sn1.take(0)
    assertTypedEquals[EmptyTuple](EmptyTuple, r1)

    val r2 = sn1.drop(0)
    assertTypedEquals[Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple](
      23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple, r2)

    val r3 = sn1.take(2)
    assertTypedEquals[Int *: Double *: EmptyTuple](23 *: 3.0 *: EmptyTuple, r3)

    val r4 = sn1.drop(2)
    assertTypedEquals[String *: Unit *: String *: Boolean *: Long *: EmptyTuple](
      "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple, r4)

    val r5 = sn1.take(7)
    assertTypedEquals[Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple](
      23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple, r5)

    val r6 = sn1.drop(7)
    assertTypedEquals[EmptyTuple](EmptyTuple, r6)
  }

  @Test
  def testSplit: Unit = {
    val sn1 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val sni0 = sn1.split(0)
    typed[(EmptyTuple, (Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](sni0)
    val sni1 = sn1.split(1)
    typed[((Int *: EmptyTuple), (Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](sni1)
    val sni2 = sn1.split(2)
    typed[((Int *: Double *: EmptyTuple), (String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](sni2)
    val sni3 = sn1.split(3)
    typed[((Int *: Double *: String *: EmptyTuple), (Unit *: String *: Boolean *: Long *: EmptyTuple))](sni3)
    val sni4 = sn1.split(4)
    typed[((Int *: Double *: String *: Unit *: EmptyTuple), (String *: Boolean *: Long *: EmptyTuple))](sni4)
    val sni5 = sn1.split(5)
    typed[((Int *: Double *: String *: Unit *: String *: EmptyTuple), (Boolean *: Long *: EmptyTuple))](sni5)
    val sni6 = sn1.split(6)
    typed[((Int *: Double *: String *: Unit *: String *: Boolean *: EmptyTuple), (Long *: EmptyTuple))](sni6)
    val sni7 = sn1.split(7)
    typed[((Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple), EmptyTuple)](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(EmptyTuple, (Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[((Int *: EmptyTuple), (Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[((Double *: Int *: EmptyTuple), (String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[((String *: Double *: Int *: EmptyTuple), (Unit *: String *: Boolean *: Long *: EmptyTuple))](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[((Unit *: String *: Double *: Int *: EmptyTuple), (String *: Boolean *: Long *: EmptyTuple))](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[((String *: Unit *: String *: Double *: Int *: EmptyTuple), (Boolean *: Long *: EmptyTuple))](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[((Boolean *: String *: Unit *: String *: Double *: Int *: EmptyTuple), (Long *: EmptyTuple))](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[((Long *: Boolean *: String *: Unit *: String *: Double *: Int *: EmptyTuple), EmptyTuple)](snri7)
  }

  @Test
  def testSplitLiteral: Unit = {
    val sn1 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val sni0 = sn1.split(0)
    typed[(EmptyTuple, (Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](sni0)
    val sni1 = sn1.split(1)
    typed[((Int *: EmptyTuple), (Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](sni1)
    val sni2 = sn1.split(2)
    typed[((Int *: Double *: EmptyTuple), (String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](sni2)
    val sni3 = sn1.split(3)
    typed[((Int *: Double *: String *: EmptyTuple), (Unit *: String *: Boolean *: Long *: EmptyTuple))](sni3)
    val sni4 = sn1.split(4)
    typed[((Int *: Double *: String *: Unit *: EmptyTuple), (String *: Boolean *: Long *: EmptyTuple))](sni4)
    val sni5 = sn1.split(5)
    typed[((Int *: Double *: String *: Unit *: String *: EmptyTuple), (Boolean *: Long *: EmptyTuple))](sni5)
    val sni6 = sn1.split(6)
    typed[((Int *: Double *: String *: Unit *: String *: Boolean *: EmptyTuple), (Long *: EmptyTuple))](sni6)
    val sni7 = sn1.split(7)
    typed[((Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple), EmptyTuple)](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(EmptyTuple, (Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[((Int *: EmptyTuple), (Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[((Double *: Int *: EmptyTuple), (String *: Unit *: String *: Boolean *: Long *: EmptyTuple))](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[((String *: Double *: Int *: EmptyTuple), (Unit *: String *: Boolean *: Long *: EmptyTuple))](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[((Unit *: String *: Double *: Int *: EmptyTuple), (String *: Boolean *: Long *: EmptyTuple))](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[((String *: Unit *: String *: Double *: Int *: EmptyTuple), (Boolean *: Long *: EmptyTuple))](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[((Boolean *: String *: Unit *: String *: Double *: Int *: EmptyTuple), (Long *: EmptyTuple))](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[((Long *: Boolean *: String *: Unit *: String *: Double *: Int *: EmptyTuple), EmptyTuple)](snri7)
  }

  @Test
  def testSplitP: Unit = {
    val sn1 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val sni0 = sn1.split(0)
    typed[(EmptyTuple) *: (Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni0)
    val sni1 = sn1.split(1)
    typed[(Int *: EmptyTuple) *: (Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni1)
    val sni2 = sn1.split(2)
    typed[(Int *: Double *: EmptyTuple) *: (String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni2)
    val sni3 = sn1.split(3)
    typed[(Int *: Double *: String *: EmptyTuple) *: (Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni3)
    val sni4 = sn1.split(4)
    typed[(Int *: Double *: String *: Unit *: EmptyTuple) *: (String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni4)
    val sni5 = sn1.split(5)
    typed[(Int *: Double *: String *: Unit *: String *: EmptyTuple) *: (Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni5)
    val sni6 = sn1.split(6)
    typed[(Int *: Double *: String *: Unit *: String *: Boolean *: EmptyTuple) *: (Long *: EmptyTuple) *: EmptyTuple](sni6)
    val sni7 = sn1.split(7)
    typed[(Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: (EmptyTuple) *: EmptyTuple](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(EmptyTuple) *: (Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[(Int *: EmptyTuple) *: (Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[(Double *: Int *: EmptyTuple) *: (String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[(String *: Double *: Int *: EmptyTuple) *: (Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[(Unit *: String *: Double *: Int *: EmptyTuple) *: (String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[(String *: Unit *: String *: Double *: Int *: EmptyTuple) *: (Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[(Boolean *: String *: Unit *: String *: Double *: Int *: EmptyTuple) *: (Long *: EmptyTuple) *: EmptyTuple](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[(Long *: Boolean *: String *: Unit *: String *: Double *: Int *: EmptyTuple) *: (EmptyTuple) *: EmptyTuple](snri7)
  }

  @Test
  def testSplitPLiteral: Unit = {
    val sn1 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val sni0 = sn1.split(0)
    typed[(EmptyTuple) *: (Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni0)
    val sni1 = sn1.split(1)
    typed[(Int *: EmptyTuple) *: (Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni1)
    val sni2 = sn1.split(2)
    typed[(Int *: Double *: EmptyTuple) *: (String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni2)
    val sni3 = sn1.split(3)
    typed[(Int *: Double *: String *: EmptyTuple) *: (Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni3)
    val sni4 = sn1.split(4)
    typed[(Int *: Double *: String *: Unit *: EmptyTuple) *: (String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni4)
    val sni5 = sn1.split(5)
    typed[(Int *: Double *: String *: Unit *: String *: EmptyTuple) *: (Boolean *: Long *: EmptyTuple) *: EmptyTuple](sni5)
    val sni6 = sn1.split(6)
    typed[(Int *: Double *: String *: Unit *: String *: Boolean *: EmptyTuple) *: (Long *: EmptyTuple) *: EmptyTuple](sni6)
    val sni7 = sn1.split(7)
    typed[(Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: (EmptyTuple) *: EmptyTuple](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(EmptyTuple) *: (Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[(Int *: EmptyTuple) *: (Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[(Double *: Int *: EmptyTuple) *: (String *: Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[(String *: Double *: Int *: EmptyTuple) *: (Unit *: String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[(Unit *: String *: Double *: Int *: EmptyTuple) *: (String *: Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[(String *: Unit *: String *: Double *: Int *: EmptyTuple) *: (Boolean *: Long *: EmptyTuple) *: EmptyTuple](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[(Boolean *: String *: Unit *: String *: Double *: Int *: EmptyTuple) *: (Long *: EmptyTuple) *: EmptyTuple](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[(Long *: Boolean *: String *: Unit *: String *: Double *: Int *: EmptyTuple) *: (EmptyTuple) *: EmptyTuple](snri7)
  }

  @Test
  def testSelect: Unit = {
    val sl = 1 *: true *: "foo" *: 2.0 *: EmptyTuple
    val si = sl.select[Int]
    assertTypedEquals[Int](1, si)

    val sb = sl.select[Boolean]
    assertTypedEquals[Boolean](true, sb)

    val ss = sl.select[String]
    assertTypedEquals[String]("foo", ss)

    val sd = sl.select[Double]
    assertEquals(2.0, sd, Double.MinPositiveValue)
  }
  @Test
  def testSelectMany: Unit = {
    val si = 1 *: true *: "foo" *: 2.0 *: EmptyTuple

    val si1 = si.selectMany[EmptyTuple]
    assertTypedEquals[EmptyTuple](EmptyTuple, si1)

    val si2 = si.selectMany[0 *: EmptyTuple]
    assertTypedEquals[Int *: EmptyTuple](1 *: EmptyTuple, si2)

    val si3 = si.selectMany[2 *: EmptyTuple]
    assertTypedEquals[String *: EmptyTuple]("foo" *: EmptyTuple, si3)

    val si4 = si.selectMany[0 *: 1 *: 2 *: 3 *: EmptyTuple]
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: true *: "foo" *: 2.0 *: EmptyTuple, si4)

    val si5 = si.selectMany(0 *: EmptyTuple)
    assertTypedEquals[Int *: EmptyTuple](1 *: EmptyTuple, si5)

    val si6 = si.selectMany(2 *: EmptyTuple)
    assertTypedEquals[String *: EmptyTuple]("foo" *: EmptyTuple, si6)

    val si7 = si.selectMany(0 *: 1 *: 2 *: EmptyTuple)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: true *: "foo" *: 2.0 *: EmptyTuple, si7)

  }
  @Test
  def testSelectRange: Unit = {
    val sl = 1 *: true *: "foo" *: 2.0 *: EmptyTuple

    val sl1  = sl.selectRange[0,0]
    val sl1i = sl.selectRange(0,0)
    assertTypedEquals[EmptyTuple](EmptyTuple, sl1)
    assertTypedEquals[EmptyTuple](EmptyTuple, sl1i)

    val sl2  = sl.selectRange[1,1]
    val sl2i = sl.selectRange(1,1)
    assertTypedEquals[EmptyTuple](EmptyTuple, sl2)
    assertTypedEquals[EmptyTuple](EmptyTuple, sl2i)

    val sl3 = sl.selectRange[0,2]
    val sl3i = sl.selectRange(0,2)
    assertTypedEquals[Int *: Boolean *: EmptyTuple](1 *: true *: EmptyTuple, sl3)
    assertTypedEquals[Int *: Boolean *: EmptyTuple](1 *: true *: EmptyTuple, sl3i)

    val sl4 = sl.selectRange[2,4]
    val sl4i = sl.selectRange(2,4)
    assertTypedEquals[String *: Double *: EmptyTuple]("foo" *: 2.0 *: EmptyTuple, sl4)
    assertTypedEquals[String *: Double *: EmptyTuple]("foo" *: 2.0 *: EmptyTuple, sl4i)

    val sl5 = sl.selectRange[0,4]
    val sl5i = sl.selectRange(0,4)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: true *: "foo" *: 2.0 *: EmptyTuple, sl5)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: true *: "foo" *: 2.0 *: EmptyTuple, sl5i)

  }

  @Test
  def testFilter: Unit = {
    val l1 = 1 *: 2 *: EmptyTuple
    val f1 = l1.filter[Int]
    assertTypedEquals[Int *: Int *: EmptyTuple](1 *: 2 *: EmptyTuple, f1)

    val l2 = 1 *: true *: "foo" *: 2 *: EmptyTuple
    val f2 = l2.filter[Int]
    assertTypedEquals[Int *: Int *: EmptyTuple](1 *: 2 *: EmptyTuple, f2)

    typed[EmptyTuple](l2.filter[Double])
  }

  @Test
  def testFilterNot: Unit = {
    val l1 = 1 *: 2 *: EmptyTuple
    val f1 = l1.filterNot[String]
    assertTypedEquals[Int *: Int *: EmptyTuple](1 *: 2 *: EmptyTuple, f1)

    val l2 = 1 *: true *: "foo" *: 2 *: EmptyTuple
    val f2 = l2.filterNot[String]
    assertTypedEquals[Int *: Boolean *: Int *: EmptyTuple](1 *: true *: 2 *: EmptyTuple, f2)

    typed[EmptyTuple](l2.filter[Double])
  }

  @Test
  def testPartition: Unit = {
    val l1 = 1 *: 2 *: EmptyTuple
    val l2 = 1 *: true *: "foo" *: 2 *: EmptyTuple

    val r1 = l1.partition[Int]
    assertTypedEquals[(Int *: Int *: EmptyTuple, EmptyTuple)]((1 *: 2 *: EmptyTuple, EmptyTuple), r1)

    val r2 = l1.partitionP[Int]
    assertTypedEquals[(Int *: Int *: EmptyTuple) *: EmptyTuple *: EmptyTuple]((1 *: 2 *: EmptyTuple) *: EmptyTuple *: EmptyTuple, r2)

    val r3 = l2.partition[Int]
    assertTypedEquals[(Int *: Int *: EmptyTuple, Boolean *: String *: EmptyTuple)]((1 *: 2 *: EmptyTuple, true *: "foo" *: EmptyTuple), r3)

    val r4 = l2.partitionP[Int]
    assertTypedEquals[(Int *: Int *: EmptyTuple) *: (Boolean *: String *: EmptyTuple) *: EmptyTuple](
      (1 *: 2 *: EmptyTuple) *: (true *: "foo" *: EmptyTuple) *: EmptyTuple, r4
    )
  }

  @Test
  def testReplace: Unit = {
    val sl = 1 *: true *: "foo" *: 2.0 *: EmptyTuple

    val (i, r1) = sl.replace(23)
    assertTypedEquals[Int](1, i)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](23 *: true *: "foo" *: 2.0 *: EmptyTuple, r1)

    val (b, r2) = sl.replace(false)
    assertTypedEquals[Boolean](true, b)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: false *: "foo" *: 2.0 *: EmptyTuple, r2)

    val (s, r3) = sl.replace("bar")
    assertTypedEquals[String]("foo", s)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: true *: "bar" *: 2.0 *: EmptyTuple, r3)

    val (d, r4) = sl.replace(3.0)
    typed[Double](d)
    assertEquals(2.0, d, Double.MinPositiveValue)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: true *: "foo" *: 3.0 *: EmptyTuple, r4)

    val (i2, r5) = sl.replaceType[Int]('*')
    typed[Char](r5(0))
    assertTypedEquals[Int](1, i2)
    assertTypedEquals[Char *: Boolean *: String *: Double *: EmptyTuple]('*' *: true *: "foo" *: 2.0 *: EmptyTuple, r5)

    val (b2, r6) = sl.replaceType[Boolean]('*')
    typed[Char](r6(1))
    assertTypedEquals[Boolean](true, b2)
    assertTypedEquals[Int *: Char *: String *: Double *: EmptyTuple](1 *: '*' *: "foo" *: 2.0 *: EmptyTuple, r6)

    val (s2, r7) = sl.replaceType[String]('*')
    typed[Char](r7(2))
    assertTypedEquals[String]("foo", s2)
    assertTypedEquals[Int *: Boolean *: Char *: Double *: EmptyTuple](1 *: true *: '*' *: 2.0 *: EmptyTuple, r7)

    val (d2, r8) = sl.replaceType[Double]('*')
    typed[Double](d2)
    typed[Char](r8(3))
    assertEquals(2.0, d2, Double.MinPositiveValue)
    assertTypedEquals[Int *: Boolean *: String *: Char *: EmptyTuple](1 *: true *: "foo" *: '*' *: EmptyTuple, r8)

    val fruits = a *: p *: a *: f *: EmptyTuple
    val (x1, rr1) = fruits.replaceType[Pear](a)
    typed[Pear](x1)
    typed[Apple *: Apple *: Apple *: Fruit *: EmptyTuple](rr1)

    val (x2, rr2) = fruits.replaceType[Pear](f)
    typed[Pear](x2)
    typed[Apple *: Fruit *: Apple *: Fruit *: EmptyTuple](rr2)

    val (x3, rr3) = fruits.replaceType[Fruit](p)
    typed[Fruit](x3)
    typed[Apple *: Pear *: Apple *: Pear *: EmptyTuple](rr3)

    val (x4, rr4) = fruits.replace(p)
    typed[Pear](x4)
    typed[Apple *: Pear *: Apple *: Fruit *: EmptyTuple](rr4)

    val (x5, rr5) = fruits.replace(f)
    typed[Fruit](x5)
    typed[Apple *: Pear *: Apple *: Fruit *: EmptyTuple](rr5)
  }

  @Test
  def testUpdate: Unit = {
    type SL = Int *: Boolean *: String *: Double *: EmptyTuple
    val sl: SL = 1 *: true *: "foo" *: 2.0 *: EmptyTuple

    val r1 = sl.updatedElem(23)
    assertTypedEquals[SL](23 *: true *: "foo" *: 2.0 *: EmptyTuple, r1)

    val r2 = sl.updatedElem(false)
    assertTypedEquals[SL](1 *: false *: "foo" *: 2.0 *: EmptyTuple, r2)

    val r3 = sl.updatedElem("bar")
    assertTypedEquals[SL](1 *: true *: "bar" *: 2.0 *: EmptyTuple, r3)

    val r4 = sl.updatedElem(3.0)
    assertTypedEquals[SL](1 *: true *: "foo" *: 3.0 *: EmptyTuple, r4)

    val r5 = sl.updatedType[Int]('*')
    assertTypedEquals[Char *: Boolean *: String *: Double *: EmptyTuple]('*' *: true *: "foo" *: 2.0 *: EmptyTuple, r5)

    val r6 = sl.updatedType[Boolean]('*')
    assertTypedEquals[Int *: Char *: String *: Double *: EmptyTuple](1 *: '*' *: "foo" *: 2.0 *: EmptyTuple, r6)

    val r7 = sl.updatedType[String]('*')
    assertTypedEquals[Int *: Boolean *: Char *: Double *: EmptyTuple](1 *: true *: '*' *: 2.0 *: EmptyTuple, r7)

    val r8 = sl.updatedType[Double]('*')
    assertTypedEquals(1 *: true *: "foo" *: '*' *: EmptyTuple, r8)

    val r9 = sl.updateWith((i: Int) => i * 2)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](2 *: true *: "foo" *: 2.0 *: EmptyTuple, r9)

    val r10 = sl.updateWith((b: Boolean) => !b)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: false *: "foo" *: 2.0 *: EmptyTuple, r10)

    val r11 = sl.updateWith((s: String) => s.toUpperCase)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: true *: "FOO" *: 2.0 *: EmptyTuple, r11)

    val r12 = sl.updateWith((d: Double) => d / 2.0)
    assertTypedEquals[Int *: Boolean *: String *: Double *: EmptyTuple](1 *: true *: "foo" *: 1.0 *: EmptyTuple, r12)

    val r13 = sl.updateWith((i: Int) => i.toString)
    assertTypedEquals[String *: Boolean *: String *: Double *: EmptyTuple]("1" *: true *: "foo" *: 2.0 *: EmptyTuple, r13)

    val r14 = sl.updateWith((b: Boolean) => b.toString)
    assertTypedEquals[Int *: String *: String *: Double *: EmptyTuple](1 *: "true" *: "foo" *: 2.0 *: EmptyTuple, r14)

    val r15 = sl.updateWith((_: String) => 0xF00)
    assertTypedEquals[Int *: Boolean *: Int *: Double *: EmptyTuple](1 *: true *: 0xF00 *: 2.0 *: EmptyTuple, r15)

    val r16 = sl.updateWith((d: Double) => d.toString)
    assertTypedEquals[Int *: Boolean *: String *: String *: EmptyTuple](1 *: true *: "foo" *: 2.0.toString *: EmptyTuple, r16)

    val fruits = a *: p *: a *: f *: EmptyTuple

    val rr1 = fruits.updatedType[Pear](a)
    typed[Apple *: Apple *: Apple *: Fruit *: EmptyTuple](rr1)

    val rr2 = fruits.updatedType[Pear](f)
    typed[Apple *: Fruit *: Apple *: Fruit *: EmptyTuple](rr2)

    val rr3 = fruits.updatedType[Fruit](p)
    typed[Apple *: Pear *: Apple *: Pear *: EmptyTuple](rr3)

    val rr4 = fruits.updatedElem(p)
    typed[Apple *: Pear *: Apple *: Fruit *: EmptyTuple](rr4)

    val rr5 = fruits.updatedElem(f)
    typed[Apple *: Pear *: Apple *: Fruit *: EmptyTuple](rr5)
  }

  @Test
  def testSplitLeft: Unit = {
    type SL  = Int *: Boolean *: String *: Double *: EmptyTuple
    type SL2 = Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple
    val sl: SL   = 1 *: true *: "foo" *: 2.0 *: EmptyTuple
    val sl2: SL2 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val (sp1, sp2) = sl.splitLeft[String]
    typed[String *: Double *: EmptyTuple](sp2)
    typed[Int *: Boolean *: EmptyTuple](sp1)
    assertTypedEquals[SL]((sp1 ::: sp2), sl)

    val (sli1, sli2) = sl2.splitLeft[String]
    typed[Int *: Double *: EmptyTuple](sli1)
    typed[String *: Unit *: String *: Boolean *: Long *: EmptyTuple](sli2)
    assertTypedEquals[SL2]((sli1 ::: sli2), sl2)

    val (rsp1, rsp2) = sl.reverse_splitLeft[String]
    typed[Boolean *: Int *: EmptyTuple](rsp1)
    typed[String *: Double *: EmptyTuple](rsp2)
    assertTypedEquals[SL]((rsp1 reverse_::: rsp2), sl)

    val (rsli1, rsli2) = sl2.reverse_splitLeft[String]
    typed[Double *: Int *: EmptyTuple](rsli1)
    typed[String *: Unit *: String *: Boolean *: Long *: EmptyTuple](rsli2)
    assertTypedEquals[SL2]((rsli1 reverse_::: rsli2), sl2)
  }

  @Test
  def testSplitLeftP: Unit = {
    type SL  = Int *: Boolean *: String *: Double *: EmptyTuple
    type SL2 = Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple
    val sl: SL   = 1 *: true *: "foo" *: 2.0 *: EmptyTuple
    val sl2: SL2 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val sp1 *: sp2 *: EmptyTuple = sl.splitLeftP[String]
    typed[String *: Double *: EmptyTuple](sp2)
    typed[Int *: Boolean *: EmptyTuple](sp1)
    assertTypedEquals[SL]((sp1 ::: sp2), sl)

    val sli1 *: sli2 *: EmptyTuple = sl2.splitLeftP[String]
    typed[Int *: Double *: EmptyTuple](sli1)
    typed[String *: Unit *: String *: Boolean *: Long *: EmptyTuple](sli2)
    assertTypedEquals[SL2]((sli1 ::: sli2), sl2)

    val rsp1 *: rsp2 *: EmptyTuple = sl.reverse_splitLeftP[String]
    typed[Boolean *: Int *: EmptyTuple](rsp1)
    typed[String *: Double *: EmptyTuple](rsp2)
    assertTypedEquals[SL]((rsp1 reverse_::: rsp2), sl)

    val rsli1 *: rsli2 *: EmptyTuple = sl2.reverse_splitLeftP[String]
    typed[Double *: Int *: EmptyTuple](rsli1)
    typed[String *: Unit *: String *: Boolean *: Long *: EmptyTuple](rsli2)
    assertTypedEquals[SL2]((rsli1 reverse_::: rsli2), sl2)
  }

  @Test
  def testSplitRight: Unit = {
    type SL  = Int *: Boolean *: String *: Double *: EmptyTuple
    type SL2 = Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple
    val sl: SL   = 1 *: true *: "foo" *: 2.0 *: EmptyTuple
    val sl2: SL2 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val (srp1, srp2) = sl.splitRight[String]
    typed[Int *: Boolean *: String *: EmptyTuple](srp1)
    typed[Double *: EmptyTuple](srp2)
    assertTypedEquals[SL]((srp1 ::: srp2), sl)

    val (srli1, srli2) = sl2.splitRight[String]
    typed[Int *: Double *: String *: Unit *: String *: EmptyTuple](srli1)
    typed[Boolean *: Long *: EmptyTuple](srli2)
    assertTypedEquals[SL2](sl2, srli1 ::: srli2)

    val (rsrp1, rsrp2) = sl.reverse_splitRight[String]
    typed[String *: Boolean *: Int *: EmptyTuple](rsrp1)
    typed[Double *: EmptyTuple](rsrp2)
    assertTypedEquals[SL]((rsrp1 reverse_::: rsrp2), sl)

    val (rsrli1, rsrli2) = sl2.reverse_splitRight[String]
    typed[String *: Unit *: String *: Double *: Int *: EmptyTuple](rsrli1)
    typed[Boolean *: Long *: EmptyTuple](rsrli2)
    assertTypedEquals[SL2]((rsrli1 reverse_::: rsrli2), sl2)
  }

  @Test
  def testSplitRightP: Unit = {
    type SL  = Int *: Boolean *: String *: Double *: EmptyTuple
    type SL2 = Int *: Double *: String *: Unit *: String *: Boolean *: Long *: EmptyTuple
    val sl: SL   = 1 *: true *: "foo" *: 2.0 *: EmptyTuple
    val sl2: SL2 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val srp1 *: srp2 *: EmptyTuple = sl.splitRightP[String]
    typed[Int *: Boolean *: String *: EmptyTuple](srp1)
    typed[Double *: EmptyTuple](srp2)
    assertTypedEquals[SL]((srp1 ::: srp2), sl)

    val srli1 *: srli2 *: EmptyTuple = sl2.splitRightP[String]
    typed[Int *: Double *: String *: Unit *: String *: EmptyTuple](srli1)
    typed[Boolean *: Long *: EmptyTuple](srli2)
    assertTypedEquals[SL2](sl2, srli1 ::: srli2)

    val rsrp1 *: rsrp2 *: EmptyTuple = sl.reverse_splitRightP[String]
    typed[String *: Boolean *: Int *: EmptyTuple](rsrp1)
    typed[Double *: EmptyTuple](rsrp2)
    assertTypedEquals[SL]((rsrp1 reverse_::: rsrp2), sl)

    val rsrli1 *: rsrli2 *: EmptyTuple = sl2.reverse_splitRightP[String]
    typed[String *: Unit *: String *: Double *: Int *: EmptyTuple](rsrli1)
    typed[Boolean *: Long *: EmptyTuple](rsrli2)
    assertTypedEquals[SL2]((rsrli1 reverse_::: rsrli2), sl2)
  }

  @Test
  def testTranspose: Unit = {
    val l1 = 1 *: EmptyTuple
    val l2 = ("a" *: EmptyTuple) *: EmptyTuple

    val r1 = l1.zipOne(l2)
    assertTypedEquals[(Int *: String *: EmptyTuple) *: EmptyTuple]((1 *: "a" *: EmptyTuple) *: EmptyTuple, r1)
    val r2 = l1.mapConst(EmptyTuple)
    assertTypedEquals[EmptyTuple *: EmptyTuple](EmptyTuple *: EmptyTuple, r2)
    val r3 = (l1 *: EmptyTuple).transpose
    assertTypedEquals[(Int *: EmptyTuple) *: EmptyTuple]((1 *: EmptyTuple) *: EmptyTuple, r3)

    val l3 = 1 *: 2 *: 3 *: EmptyTuple
    val l4 = ("a" *: 1.0 *: EmptyTuple) *: ("b" *: 2.0 *: EmptyTuple) *: ("c" *: 3.0 *: EmptyTuple) *: EmptyTuple

    type ISD = Int *: String *: Double *: EmptyTuple
    val z2 = l3.zipOne(l4)
    assertTypedEquals[ISD *: ISD *: ISD *: EmptyTuple](
      (1 *: "a" *: 1.0 *: EmptyTuple) *: (2 *: "b" *: 2.0 *: EmptyTuple) *: (3 *: "c" *: 3.0 *: EmptyTuple) *: EmptyTuple, z2
    )

    val r5 = l3.mapConst(EmptyTuple)
    assertTypedEquals[EmptyTuple *: EmptyTuple *: EmptyTuple *: EmptyTuple](EmptyTuple *: EmptyTuple *: EmptyTuple *: EmptyTuple, r5)

    val t2 = l4.transpose
    assertTypedEquals[
      (String *: String *: String *: EmptyTuple) ::
      (Double *: Double *: Double *: EmptyTuple) *: EmptyTuple
    ](("a" *: "b" *: "c" *: EmptyTuple) *: (1.0 *: 2.0 *: 3.0 *: EmptyTuple) *: EmptyTuple, t2)

    val t3 = z2.transpose
    assertTypedEquals[
      (Int *: Int *: Int *: EmptyTuple) ::
      (String *: String *: String *: EmptyTuple) ::
      (Double *: Double *: Double *: EmptyTuple) *: EmptyTuple
    ](
      (1 *: 2 *: 3 *: EmptyTuple) ::
      ("a" *: "b" *: "c" *: EmptyTuple) ::
      (1.0 *: 2.0 *: 3.0 *: EmptyTuple) *: EmptyTuple,
      t3
    )

    val r8 = t3.transpose
    assertTypedEquals[ISD *: ISD *: ISD *: EmptyTuple](z2, r8)

    val nil: EmptyTuple = EmptyTuple

    val r9 = nil zipOne nil
    assertTypedEquals[EmptyTuple](EmptyTuple, r9)

    val r10 = nil.transpose
    assertTypedEquals[EmptyTuple](EmptyTuple, r10)

    val r11 = (EmptyTuple *: EmptyTuple *: EmptyTuple: EmptyTuple *: EmptyTuple *: EmptyTuple).transpose
    assertTypedEquals[EmptyTuple](EmptyTuple, r11)

    val r12 = (1 *: EmptyTuple) zipOne ((2 *: EmptyTuple) *: EmptyTuple)
    assertTypedEquals[(Int *: Int *: EmptyTuple) *: EmptyTuple]((1 *: 2 *: EmptyTuple) *: EmptyTuple, r12)
  }

  @Test
  def testZipUnzip: Unit = {
    val l1 = 1 *: "a" *: 1.0 *: EmptyTuple
    val l2 = 2 *: "b" *: 2.0 *: EmptyTuple

    val t1 = (l1 *: l2 *: EmptyTuple).transpose
    val z1 = t1.map(tupled)
    assertTypedEquals[(Int, Int) *: (String, String) *: (Double, Double) *: EmptyTuple](
      (1, 2) *: ("a", "b") *: (1.0, 2.0) *: EmptyTuple, z1)

    def zip[L <: Tuple, OutT <: Tuple](l: L)
      (implicit
        transposer: Transposer.Aux[L, OutT],
        mapper: Mapper[tupled.type, OutT]) = l.transpose.map(tupled)

    val z2 = zip(l1 *: l2 *: EmptyTuple)
    assertTypedEquals[(Int, Int) *: (String, String) *: (Double, Double) *: EmptyTuple](
      (1, 2) *: ("a", "b") *: (1.0, 2.0) *: EmptyTuple, z2)

    val z3 = (l1 *: l2 *: EmptyTuple).zip
    assertTypedEquals[(Int, Int) *: (String, String) *: (Double, Double) *: EmptyTuple](
      (1, 2) *: ("a", "b") *: (1.0, 2.0) *: EmptyTuple, z3)

    val nil: EmptyTuple = EmptyTuple
    val z4 = (nil *: nil *: EmptyTuple).zip
    assertTypedEquals[EmptyTuple](nil, z4)

    val t2 = z1.map(productElements).transpose
    val u1 = t2.tupled
    assertTypedEquals[(Int *: String *: Double *: EmptyTuple, Int *: String *: Double *: EmptyTuple)](
      (1 *: "a" *: 1.0 *: EmptyTuple, 2 *: "b" *: 2.0 *: EmptyTuple), u1)

    def unzip[L <: Tuple, OutM <: Tuple, OutT <: Tuple](l: L)
      (implicit
        mapper: Mapper.Aux[productElements.type, L, OutM],
        transposer: Transposer.Aux[OutM, OutT],
        tupler: Tupler[OutT]) = l.map(productElements).transpose.tupled

    val u2 = unzip(z1)
    assertTypedEquals[(Int *: String *: Double *: EmptyTuple, Int *: String *: Double *: EmptyTuple)](
      (1 *: "a" *: 1.0 *: EmptyTuple, 2 *: "b" *: 2.0 *: EmptyTuple), u2)

    val r1 = z1.unzip
    assertTypedEquals[(Int *: String *: Double *: EmptyTuple, Int *: String *: Double *: EmptyTuple)](
      (1 *: "a" *: 1.0 *: EmptyTuple, 2 *: "b" *: 2.0 *: EmptyTuple), r1)

    val r2 = l1 zip l2
    assertTypedEquals[(Int, Int) *: (String, String) *: (Double, Double) *: EmptyTuple](
      (1, 2) *: ("a", "b") *: (1.0, 2.0) *: EmptyTuple, r2)

    val intInc: Int => Int = _+1
    val stringInc: String => String = _+"*"
    val doubleInc: Double => Int = _.toInt+1

    val l3 = intInc *: stringInc *: doubleInc *: EmptyTuple

    val z5 = l3 zipApply l1
    assertTypedEquals[Int *: String *: Int *: EmptyTuple](2 *: "a*" *: 2 *: EmptyTuple, z5)
  }

  @Test
  def testUnapply: Unit = {
    val l = 1 *: true *: "foo" *: 2.0 *: EmptyTuple
    val l2 = 23 *: 3.0 *: "foo" *: () *: "bar" *: true *: 5L *: EmptyTuple

    val is = l match {
      case i *: true *: s *: 2.0 *: EmptyTuple => (i, s)
      case _ => sys.error("Not matched")
    }

    assertTypedEquals[Int](1, is._1)
    assertTypedEquals[String]("foo", is._2)

    val is2 = (l: Any) match {
      case (i: Int) *: true *: (s: String) *: 2.0 *: EmptyTuple => (i, s)
      case _ => sys.error("Not matched")
    }

    assertTypedEquals[Int](1, is2._1)
    assertTypedEquals[String]("foo", is2._2)

    import Tuple.ListCompat._

    val tl = l2 match {
      case 23 #: 3.0 #: s #: xs => (s, xs)
      case _ => sys.error("Not matched")
    }

    assertTypedEquals[String]("foo", tl._1)
    assertTypedEquals[Unit *: String *: Boolean *: Long *: EmptyTuple](() *: "bar" *: true *: 5L *: EmptyTuple, tl._2)

    val tl2 = (l2: Any) match {
      case 23 #: 3.0 #: (s: String) #: xs => (s, xs)
      case _ => sys.error("Not matched")
    }

    assertTypedEquals[String]("foo", tl2._1)
    assertTypedEquals[Tuple](() *: "bar" *: true *: 5L *: EmptyTuple, tl2._2)

    val ll = List(1, 2, 3, 4)
    val tll = ll match {
      case List(1, 2, x, y) => (x, y)
      case _ => sys.error("Not matched")
    }
    assertTypedEquals[Int](3, tll._1)
    assertTypedEquals[Int](4, tll._2)

    val tll2 = ll match {
      case 1 *: xs => xs
      case _ => sys.error("Not matched")
    }
    assertTypedEquals[List[Int]](List(2, 3, 4), tll2)

    val mixed = 23 *: "foo" *: List(1, 2, 3, 4, 5) *: false *: () *: EmptyTuple
    val tmixed = mixed match {
      case _ #: _ #: (_ *: 2 *: x *: tl1) #: tl2 => (x, tl1, tl2)
      case _ => sys.error("Not matched")
    }
    assertTypedEquals[Int](3, tmixed._1)
    assertTypedEquals[List[Int]](List(4, 5), tmixed._2)
    assertTypedEquals[Boolean *: Unit *: EmptyTuple](false *: () *: EmptyTuple, tmixed._3)
  }

  @Test
  def testRemove: Unit = {
    val l = 1 *: true *: "foo" *: EmptyTuple

    val li = l.removeElem[Int]
    assertTypedEquals[(Int, Boolean *: String *: EmptyTuple)]((1, true *: "foo" *: EmptyTuple), li)

    val lb = l.removeElem[Boolean]
    assertTypedEquals[(Boolean, Int *: String *: EmptyTuple)]((true, 1 *: "foo" *: EmptyTuple), lb)

    val ls = l.removeElem[String]
    assertTypedEquals[(String, Int *: Boolean *: EmptyTuple)](("foo", 1 *: true *: EmptyTuple), ls)

    val withDuplicates = 1 *: 'a' *: 'b' *: EmptyTuple
    val remover = implicitly[Remove.Aux[Int *: Char *: Char *: EmptyTuple, Char, (Char, Int *: Char *: EmptyTuple)]]
    assertTypedEquals[(Char, Int *: Char *: EmptyTuple)](('a', 1 *: 'b' *: EmptyTuple), remover(withDuplicates))
  }

  @Test
  def testRemoveAll: Unit = {
    val l = 1 *: true *: "foo" *: EmptyTuple

    val lnil = l.removeAll[EmptyTuple]
    assertTypedEquals[(EmptyTuple, Int *: Boolean *: String *: EmptyTuple)]((EmptyTuple, 1 *: true *: "foo" *: EmptyTuple), lnil)

    val li = l.removeAll[Int *: EmptyTuple]
    assertTypedEquals[(Int *: EmptyTuple, Boolean *: String *: EmptyTuple)]((1 *: EmptyTuple, true *: "foo" *: EmptyTuple), li)

    val lb = l.removeAll[Boolean *: EmptyTuple]
    assertTypedEquals[(Boolean *: EmptyTuple, Int *: String *: EmptyTuple)]((true *: EmptyTuple, 1 *: "foo" *: EmptyTuple), lb)

    val lbi = l.removeAll[Boolean *: Int *: EmptyTuple]
    assertTypedEquals[(Boolean *: Int *: EmptyTuple, String *: EmptyTuple)]((true *: 1 *: EmptyTuple, "foo" *: EmptyTuple), lbi)
  }

  @Test
  def testUnion: Unit = {
    type L1 = String *: Long *: EmptyTuple
    val l1: L1 = "foo" *: 3L *: EmptyTuple

    type L2 = Int *: String *: Boolean *: EmptyTuple
    val l2: L2 = 2 *: "bar" *: true *: EmptyTuple

    type L3 = Int *: Int *: EmptyTuple
    val l3: L3 = 1 *: 2 *: EmptyTuple

    type L4 = Int *: Int *: Int *: EmptyTuple
    val l4: L4 = 4 *: 5 *: 6 *: EmptyTuple

    val lnil = l1.union[EmptyTuple](EmptyTuple)
    assertTypedEquals[L1](l1, lnil)

    val lself = l1.union(l1)
    assertTypedEquals[L1](l1, lself)

    val l12 = l1.union(l2)
    assertTypedEquals[String *: Long *: Int *: Boolean *: EmptyTuple]("foo" *: 3L *: 2 *: true *: EmptyTuple, l12)

    val l21 = l2.union(l1)
    assertTypedEquals[Int *: String *: Boolean *: Long *: EmptyTuple](2 *: "bar" *: true *: 3L *: EmptyTuple, l21)


    illTyped { """implicitly[Union.Aux[Int *: EmptyTuple, Int *: EmptyTuple, Int *: Int *: EmptyTuple]]"""}

    val ldup1 = (l3).union(l4)
    assertTypedEquals[Int *: Int *: Int *: EmptyTuple](1 *: 2 *: 6 *: EmptyTuple, ldup1)

    val ldup2 = (l4).union(l3)
    assertTypedEquals[Int *: Int *: Int *: EmptyTuple](4 *: 5 *: 6 *: EmptyTuple, ldup2)
  }

  @Test
  def testIntersection: Unit = {
    type L1 = String *: Long *: Int *: EmptyTuple
    val l1: L1 = "foo" *: 1L *: 3 *: EmptyTuple

    type L2 = Int *: String *: Boolean *: EmptyTuple
    val l2: L2 = 2 *: "bar" *: true *: EmptyTuple

    type L3 = Int *: String *: Int *: EmptyTuple
    val l3: L3 = 4 *: "foo" *: 5 *: EmptyTuple

    val lnil = l1.intersect[EmptyTuple]
    assertTypedEquals[EmptyTuple](EmptyTuple, lnil)

    val lself = l1.intersect[L1]
    assertTypedEquals[L1](l1, lself)

    val l12 = l1.intersect[L2]
    assertTypedEquals[String *: Int *: EmptyTuple]("foo" *: 3 *: EmptyTuple, l12)

    val l21 = l2.intersect[L1]
    assertTypedEquals[Int *: String *: EmptyTuple](2 *: "bar" *: EmptyTuple, l21)

    illTyped { """implicitly[Intersection.Aux[Int *: EmptyTuple, Int *: EmptyTuple, EmptyTuple]]"""}

    val ldup1 = (l3).intersect[Int *: EmptyTuple]
    assertTypedEquals[Int *: EmptyTuple](4 *: EmptyTuple, ldup1)

    val ldup2 = (l3).intersect[Int *: Int *: EmptyTuple]
    assertTypedEquals[Int *: Int *: EmptyTuple](4 *: 5 *: EmptyTuple, ldup2)

    val ldup3 = (l3).intersect[String *: EmptyTuple]
    assertTypedEquals[String *: EmptyTuple]("foo" *: EmptyTuple, ldup3)
  }

  @Test
  def testDiff: Unit = {
    type L1 = String *: Long *: Int *: EmptyTuple
    val l1: L1 = "foo" *: 1L *: 3 *: EmptyTuple

    type L2 = Int *: String *: Boolean *: EmptyTuple
    val l2: L2 = 2 *: "bar" *: true *: EmptyTuple

    type L3 = Int *: Boolean *: Int *: EmptyTuple
    val l3: L3 = 4 *: false *: 5 *: EmptyTuple

    val lnil = l1.diff[EmptyTuple]
    assertTypedEquals[L1](l1, lnil)

    val lself = l1.diff[L1]
    assertTypedEquals[EmptyTuple](EmptyTuple, lself)

    val l12 = l1.diff[L2]
    assertTypedEquals[Long *: EmptyTuple](1L *: EmptyTuple, l12)

    val l21 = l2.diff[L1]
    assertTypedEquals[Boolean *: EmptyTuple](true *: EmptyTuple, l21)

    val ldup1 = (l3).diff[Int *: EmptyTuple]
    assertTypedEquals[Boolean *: Int *: EmptyTuple](false *: 5 *: EmptyTuple, ldup1)

    val ldup2 = (l3).diff[Int *: Int *: EmptyTuple]
    assertTypedEquals[Boolean *: EmptyTuple](false *: EmptyTuple, ldup2)

    val ldup3 = (l3).diff[Boolean *: EmptyTuple]
    assertTypedEquals[Int *: Int *: EmptyTuple](4 *: 5 *: EmptyTuple, ldup3)
  }

  @Test
  def testReinsert: Unit = {
    type L = Int *: Boolean *: String *: EmptyTuple

    val l: L = 1 *: true *: "foo" *: EmptyTuple

    val (i, li) = l.removeElem[Int]
    assertTypedEquals[L](li.reinsert[L](i), l)

    val (b, lb) = l.removeElem[Boolean]
    assertTypedEquals[L](lb.reinsert[L](b), l)

    val (s, ls) = l.removeElem[String]
    assertTypedEquals[L](ls.reinsert[L](s), l)
  }

  @Test
  def testReinsertAll: Unit = {
    type L = Int *: Boolean *: String *: EmptyTuple

    val l = 1 *: true *: "foo" *: EmptyTuple

    val (nil, lnil) = l.removeAll[EmptyTuple]
    assertTypedEquals[L](lnil.reinsertAll[L](nil), l)

    val (i, li) = l.removeAll[Int *: EmptyTuple]
    assertTypedEquals[L](li.reinsertAll[L](i), l)

    val (b, lb) = l.removeAll[Boolean *: EmptyTuple]
    assertTypedEquals[L](lb.reinsertAll[L](b), l)

    val (bi, lbi) = l.removeAll[Boolean *: Int *: EmptyTuple]
    assertTypedEquals[L](lbi.reinsertAll[L](bi), l)
  }

  object combine extends Poly2 {
    given caseCharString: Case.Aux[Char, String, Int] = at((c: Char, s: String) => s.indexOf(c))
    given caseIntBoolean: Case.Aux[Int, Boolean, String] = at((i: Int, b: Boolean) => if ((i >= 0) == b) "pass" else "fail")
  }

  @Test
  def testFoldLeft: Unit = {
    val c1a = combine('o', "foo")
    val c1b = combine(c1a, true)
    assertTypedEquals[String]("pass", c1b)

    implicitly[LeftFolder.Aux[EmptyTuple, String, combine.type, String]]
    implicitly[LeftFolder.Aux[Boolean *: EmptyTuple, Int, combine.type, String]]
    implicitly[LeftFolder.Aux[String *: Boolean *: EmptyTuple, Char, combine.type, String]]

    val tf1 = implicitly[LeftFolder[EmptyTuple, String, combine.type]]
    val tf2 = implicitly[LeftFolder[Boolean *: EmptyTuple, Int, combine.type]]
    val tf3 = implicitly[LeftFolder[String *: Boolean *: EmptyTuple, Char, combine.type]]

    val l1 = "foo" *: true *: EmptyTuple
    val f1 = l1.foldLeft('o')(combine)
    assertTypedEquals[String]("pass", f1)

    val c2a = combine('o', "bar")
    val c2b = combine(c2a, false)
    assertTypedEquals[String]("pass", c2b)

    val l2 = "bar" *: false *: EmptyTuple
    val f2 = l2.foldLeft('o')(combine)
    assertTypedEquals[String]("pass", f2)
  }

  @Test
  def testUpdatedAt: Unit = {
    type IBS = Int *: Boolean *: String *: EmptyTuple
    val l = 1 *: true *: "foo" *: EmptyTuple

    val r1 = l.updatedAt[0](2)
    assertTypedEquals[IBS](2 *:  true *: "foo" *: EmptyTuple, r1)

    val r2 = l.updatedAt[1](false)
    assertTypedEquals[IBS](1 *: false *: "foo" *: EmptyTuple, r2)

    val r3 = l.updatedAt[2]("bar")
    assertTypedEquals[IBS](1 *:  true *: "bar" *: EmptyTuple, r3)
  }

  @Test
  def testUpdatedAtLiteral: Unit = {
    type IBS = Int *: Boolean *: String *: EmptyTuple
    val l = 1 *: true *: "foo" *: EmptyTuple

    val r1 = l.updatedAt(0, 2)
    assertTypedEquals[IBS](2 *:  true *: "foo" *: EmptyTuple, r1)

    val r2 = l.updatedAt(1, false)
    assertTypedEquals[IBS](1 *: false *: "foo" *: EmptyTuple, r2)

    val r3 = l.updatedAt(2, "bar")
    assertTypedEquals[IBS](1 *:  true *: "bar" *: EmptyTuple, r3)
  }

  // @Test
  // def testNatTRel: Unit = {
  //   type L1 = Int *: String *: Boolean *: EmptyTuple
  //   type L2 = List[Int] *: List[String] *: List[Boolean] *: EmptyTuple
  //   type L3 = Option[Int] *: Option[String] *: Option[Boolean] *: EmptyTuple
  //   type L4 = Int *: Int *: Int *: EmptyTuple
  //   type L5 = String *: String *: String *: EmptyTuple

  //   implicitly[NatTRel[L1, Id, L2, List]]
  //   implicitly[NatTRel[L2, List, L1, Id]]

  //   implicitly[NatTRel[L2, List, L3, Option]]

  //   implicitly[NatTRel[L1, Id, L4, [a] =>> Int]]

  //   implicitly[NatTRel[L2, List, L4, [a] =>> Int]]
  // }

  // object optionToList extends (Option ~> List) {
  //   def apply[A](fa: Option[A]): List[A] = List.fill(3)(fa.toList).flatten
  // }

  // @Test
  // def testNatTRelMap: Unit = {
  //   type L1 = Option[Int] *: Option[Boolean] *: Option[String] *: Option[Nothing] *: EmptyTuple
  //   type L2 = List[Int] *: List[Boolean] *: List[String] *: List[Nothing] *: EmptyTuple
  //   val nattrel = implicitly[NatTRel[L1, Option, L2, List]]

  //   val l1: L1 = Option(1) *: Option(true) *: Option("three") *: None *: EmptyTuple
  //   val l2 = nattrel.map(optionToList, l1)

  //   assertTypedEquals[L2](l2,
  //     List(1, 1, 1) *: List(true, true, true) *: List("three", "three", "three") *: List() *: EmptyTuple)
  // }

  @Test
  def testZipConst: Unit = {
    type IBS = Int *: Boolean *: String *: EmptyTuple
    val c = 5
    type WithConst = (Int, Int) *: (Boolean, Int) *: (String, Int) *: EmptyTuple
    val l = 1 *: true *: "a" *: EmptyTuple
    typed[IBS](l)
    val expected = (1, c) *: (true, c) *: ("a", c) *: EmptyTuple

    val zcIntIbs = ZipConst[Int, IBS]
    val zipped1 = zcIntIbs(c, l)
    assertTypedEquals[WithConst](expected, zipped1)

    val zcaIntIbs = implicitly[ZipConst.Aux[Int, IBS, WithConst]]
    assertTypedEquals[WithConst](expected, zcaIntIbs(c, l))

    val x = l.zipConst(c)
    assertTypedEquals[WithConst](expected, x)

    Tuple().zipConst("")
  }

  @Test
  def testZipWith: Unit = {
    import poly._

    object empty extends Poly2

    object add extends Poly2 {
      given caseIntInt: Case.Aux[Int, Int, Int] = at[Int, Int](_ + _)
    }

    // EmptyTuple zipWith EmptyTuple (emptyFn)
    val r1 = (EmptyTuple: EmptyTuple).zipWith(EmptyTuple: EmptyTuple)(empty)
    assertTypedEquals[EmptyTuple](EmptyTuple, r1)

    // EmptyTuple zipWith nonEmpty (emptyFn)
    val r2 = (EmptyTuple: EmptyTuple).zipWith(1 *: EmptyTuple)(empty)
    assertTypedEquals[EmptyTuple](EmptyTuple, r2)

    // nonEmpty zipWith EmptyTuple (emptyFn)
    val r3 = (1 *: EmptyTuple).zipWith(EmptyTuple: EmptyTuple)(empty)
    assertTypedEquals[EmptyTuple](EmptyTuple, r3)

    // singleton zipWith singleton
    val r4 = (1 *: EmptyTuple).zipWith(2 *: EmptyTuple)(add)
    assertTypedEquals[Int *: EmptyTuple](3 *: EmptyTuple, r4)

    { // longList zipWith longerList
      type Left  = Int *: String *: Double *: EmptyTuple
      type Right = Int *: Double *: String *: Boolean *: EmptyTuple

      val left: Left   = 1 *: "foo" *: 1.2 *: EmptyTuple
      val right: Right = 2 *: 2.3 *: "3.4" *: true *: EmptyTuple

      object zipFn extends Poly2 {
        given caseIntInt: Case.Aux[Int, Int, Int] = at[Int, Int](_ + _)
        given caseStringDouble: Case.Aux[String, Double, String] = at[String, Double](_ + " -> " + _.toString)
        given caseDoubleString: Case.Aux[Double, String, Double] = at[Double, String](_ + _.toDouble)
      }

      val r5 = left.zipWith(right)(zipFn)
      assertTypedEquals[Int *: String *: Double *: EmptyTuple](3 *: "foo -> 2.3" *: 4.6 *: EmptyTuple, r5)
    }

    def testZipWithIndex: Unit = {

      // EmptyTuple zipWithIndex
      val r1 = (EmptyTuple: EmptyTuple).zipWithIndex
      assertTypedEquals[EmptyTuple](EmptyTuple, r1)

      // One element Tuple zipWithIndex
      val r2 = (0 *: EmptyTuple).zipWithIndex
      assertTypedEquals[(Int,0)::EmptyTuple]((0,0)::EmptyTuple, r2)

      // Tuple zipWithIndex
      val r3 = (0 *: 1 *: 2 *: 3 *: EmptyTuple).zipWithIndex
      assertTypedEquals[(Int,0)::(Int,1)::(Int,2)::(Int,3)::EmptyTuple]((0,0)::(1,1)::(2,2)::(3,3)::EmptyTuple, r3)

    }

    { // invalid polys
      illTyped("""
        (1 *: EmptyTuple).zipWith(2 *: EmptyTuple)(empty)
      """)

      object noIntFn extends Poly2 {
        given caseDoubleDouble: Case.Aux[Double, Double, Double] = at[Double, Double](_ + _)
      }

      illTyped("""
        (1 *: EmptyTuple).zipWith(2 *: EmptyTuple)(noIntFn)
      """)

      illTyped("""
        (1.0 *: 2 *: EmptyTuple).zipWith(2.0 *: 3 *: EmptyTuple)(noIntFn)
      """)
    }
  }

  @Test
  def testWithKeys: Unit = {
    import record._
    import syntax.singleton._

    val orig =
      ("intField" ->> 1) ::
      ("boolField" ->> true) ::
      EmptyTuple

    val result = orig.values.zipWithKeys(orig.keys)
    sameTyped(orig)(result)
    assertEquals(orig, result)
    val int = result.get("intField")
    assertTypedEquals[Int](1, int)
    val bool = result.get("boolField")
    assertTypedEquals[Boolean](true, bool)
    illTyped("""result.get("otherField")""")

    // key/value lengths must match up
    illTyped("orig.tail.values.zipWithKeys(orig.keys)")
    illTyped("orig.values.zipWithKeys(orig.keys.tail)")

    // Explicit type argument
    {
      val result = orig.values.zipWithKeys[Tuple.`"intField", "boolField"`.T]
      sameTyped(orig)(result)
      assertEquals(orig, result)
      val int = result.get("intField")
      assertTypedEquals[Int](1, int)
      val bool = result.get("boolField")
      assertTypedEquals[Boolean](true, bool)
      illTyped("""result.get("otherField")""")

      // key/value lengths must match up
      illTyped(""" orig.tail.values.zipWithKeys[Tuple.`"intField", "boolField"`.T] """)
      illTyped(""" orig.values.zipWithKeys[Tuple.`"boolField"`.T] """)
    }
  }

  @Test
  def testCollect: Unit = {
    import poly._

    object empty extends Poly1

    object complex extends Poly1 {
      given caseInt: Case.Aux[Int, Double] = at[Int](_.toDouble)
      given caseString: Case.Aux[String, Int] = at[String](_ => 1)
    }

    val in: Int *: String *: Double *: EmptyTuple = 1 *: "foo" *: 2.2 *: EmptyTuple

    // EmptyTuple collect p
    val r1 = (EmptyTuple: EmptyTuple).collect(empty)
    assertTypedEquals[EmptyTuple](EmptyTuple, r1)

    val r2 = (EmptyTuple: EmptyTuple).collect(id)
    assertTypedEquals[EmptyTuple](EmptyTuple, r2)

    val r3 = (EmptyTuple: EmptyTuple).collect(complex)
    assertTypedEquals[EmptyTuple](EmptyTuple, r3)

    // non-EmptyTuple collect empty
    val r4 = in.collect(empty)
    assertTypedEquals[EmptyTuple](EmptyTuple, r4)

    // non-EmptyTuple collect identity
    val r5 = in.collect(id)
    assertTypedEquals[Int *: String *: Double *: EmptyTuple](in, r5)

    // non-EmptyTuple collect complex
    val r6 = in.collect(complex)
    assertTypedEquals[Double *: Int *: EmptyTuple](1.0 *: 1 *: EmptyTuple, r6)
  }

  @Test
  def testOrdering: Unit = {
    assertEquals(List(EmptyTuple: EmptyTuple, EmptyTuple), List(EmptyTuple: EmptyTuple, EmptyTuple).sorted)

    assertEquals(List(1 *: EmptyTuple, 2 *: EmptyTuple, 3 *: EmptyTuple), List(2 *: EmptyTuple, 1 *: EmptyTuple, 3 *: EmptyTuple).sorted)

    assertEquals(
      List(1 *: "abc" *: EmptyTuple, 1 *: "def" *: EmptyTuple, 2 *: "abc" *: EmptyTuple, 2 *: "def" *: EmptyTuple),
      List(2 *: "abc" *: EmptyTuple, 1 *: "def" *: EmptyTuple, 2 *: "def" *: EmptyTuple, 1 *: "abc" *: EmptyTuple).sorted
    )
  }

  @Test
  def testMapCons: Unit = {
    type C = Char; type S = String; type I = Int; type D = Double

    val r1 = (EmptyTuple: EmptyTuple).mapCons('a')
    assertTypedEquals[EmptyTuple](EmptyTuple, r1)

    val r2 = (EmptyTuple *: EmptyTuple).mapCons('a')
    assertTypedEquals[(Char *: EmptyTuple) *: EmptyTuple]((('a' *: EmptyTuple) *: EmptyTuple), r2)

    val r3 = ((1 *: EmptyTuple) *: ("foo" *: EmptyTuple) *: (2.0 *: EmptyTuple) *: EmptyTuple).mapCons('a')
    assertTypedEquals[(C *: I *: EmptyTuple) *: (C *: S *: EmptyTuple) *: (C *: D *: EmptyTuple) *: EmptyTuple](
      ('a' *: 1 *: EmptyTuple) *: ('a' *: "foo" *: EmptyTuple) *: ('a' *: 2.0 *: EmptyTuple) *: EmptyTuple,
      r3
    )
  }

  @Test
  def testInterleave: Unit = {
    type C = Char; type S = String; type I = Int; type D = Double
    def interleave[I, L <: Tuple](i: I, l: L)(implicit interleave: Interleave[I, L]): interleave.Out = interleave(i, l)

    val r1 = interleave('i', EmptyTuple)
    assertTypedEquals[(Char *: EmptyTuple) *: EmptyTuple](('i' *: EmptyTuple) *: EmptyTuple, r1)

    val r2 = interleave('i', 1 *: EmptyTuple)
    assertTypedEquals[(C *: I *: EmptyTuple) *: (I *: C *: EmptyTuple) *: EmptyTuple](('i' *: 1 *: EmptyTuple) *: (1 *: 'i' *: EmptyTuple) *: EmptyTuple,
      r2
    )

    val r3 = interleave('i', 1 *: "foo" *: EmptyTuple)
    assertTypedEquals[(C *: I *: S *: EmptyTuple) *: (I *: C *: S *: EmptyTuple) *: (I *: S *: C *: EmptyTuple) *: EmptyTuple](
      ('i' *: 1 *: "foo" *: EmptyTuple) ::
      (1 *: 'i' *: "foo" *: EmptyTuple) ::
      (1 *: "foo" *: 'i' *: EmptyTuple) *: EmptyTuple,
      r3
    )

    val r4 = interleave('i', 1 *: "foo" *: 2.0 *: EmptyTuple)
    assertTypedEquals[(C *: I *: S *: D *: EmptyTuple) *: (I *: C *: S *: D *: EmptyTuple) *: (I *: S *: C *: D *: EmptyTuple) *: (I *: S *: D *: C *: EmptyTuple) *: EmptyTuple](
      ('i' *: 1 *: "foo" *: 2.0 *: EmptyTuple) ::
      (1 *: 'i' *: "foo" *: 2.0 *: EmptyTuple) ::
      (1 *: "foo" *: 'i' *: 2.0 *: EmptyTuple) ::
      (1 *: "foo" *: 2.0 *: 'i' *: EmptyTuple) *: EmptyTuple,
      r4
    )
  }

  @Test
  def testFlatMapInterleave: Unit = {
    type C = Char; type I = Int

    def flatMapInterleave[I, L <: Tuple](i: I, l: L)(implicit flatMapInterleave: FlatMapInterleave[I, L]) =
      flatMapInterleave(i, l)

    val r1 = flatMapInterleave('i', EmptyTuple)
    assertTypedEquals[EmptyTuple](EmptyTuple, r1)

    val r2 = flatMapInterleave('i', EmptyTuple *: EmptyTuple)
    assertTypedEquals[(Char *: EmptyTuple) *: EmptyTuple](('i' *: EmptyTuple) *: EmptyTuple, r2)

    val r3 = flatMapInterleave('i', (1 *: EmptyTuple) *: (2 *: EmptyTuple) *: EmptyTuple)
    assertTypedEquals[(C *: I *: EmptyTuple) *: (I *: C *: EmptyTuple) *: (C *: I *: EmptyTuple) *: (I *: C *: EmptyTuple) *: EmptyTuple](
      ('i' *: 1 *: EmptyTuple) ::
      (1 *: 'i' *: EmptyTuple) ::
      ('i' *: 2 *: EmptyTuple) ::
      (2 *: 'i' *: EmptyTuple) *: EmptyTuple,
      r3
    )
  }

  @Test
  def testPermutations: Unit = {
    type S = String; type I = Int; type D = Double

    val r1 = EmptyTuple.permutations
    assertTypedEquals[EmptyTuple *: EmptyTuple](EmptyTuple *: EmptyTuple, r1)

    val r2 = (1 *: EmptyTuple).permutations
    assertTypedEquals[(Int *: EmptyTuple) *: EmptyTuple]((1 *: EmptyTuple) *: EmptyTuple, r2)

    val r3 = (1 *: "foo" *: EmptyTuple).permutations
    assertTypedEquals[(I *: S *: EmptyTuple) *: (S *: I *: EmptyTuple) *: EmptyTuple](
      (1 *: "foo" *: EmptyTuple) ::
      ("foo" *: 1 *: EmptyTuple) *: EmptyTuple,
      r3
    )

    val r4 = (1 *: "foo" *: 2.0 *: EmptyTuple).permutations
    assertTypedEquals[
      (I *: S *: D *: EmptyTuple) *: (S *: I *: D *: EmptyTuple) *: (S *: D *: I *: EmptyTuple) ::
      (I *: D *: S *: EmptyTuple) *: (D *: I *: S *: EmptyTuple) *: (D *: S *: I *: EmptyTuple) *: EmptyTuple
    ](
      (1 *: "foo" *: 2.0 *: EmptyTuple) ::
      ("foo" *: 1 *: 2.0 *: EmptyTuple) ::
      ("foo" *: 2.0 *: 1 *: EmptyTuple) ::
      (1 *: 2.0 *: "foo" *: EmptyTuple) ::
      (2.0 *: 1 *: "foo" *: EmptyTuple) ::
      (2.0 *: "foo" *: 1 *: EmptyTuple) *: EmptyTuple,
      r4
    )
  }

  @Test
  def testMkString: Unit = {
    assertEquals(s"⸨1, foo, ${2.0}⸩", (1 *: "foo" *: 2.0 *: EmptyTuple).mkString("⸨", ", ", "⸩"))
  }

  @Test
  def testRotateLeft: Unit = {
    val in0 = EmptyTuple
    val in1 = 1 *: EmptyTuple
    val in2 = 1 *: "foo" *: EmptyTuple
    val in3 = 1 *: "foo" *: 2.0 *: EmptyTuple
    val in4 = 1 *: "foo" *: 2.0 *: 'a' *: EmptyTuple
    type S = String; type I = Int; type D = Double; type C = Char

    { // rotateLeft(0)
      val r1 = in0.rotateLeft(0)
      assertTypedSame[EmptyTuple](EmptyTuple, r1)
      val r2 = in1.rotateLeft(0)
      assertTypedSame[I *: EmptyTuple](in1, r2)
      val r3 = in2.rotateLeft(0)
      assertTypedSame[I *: S *: EmptyTuple](in2, r3)
      val r4 = in3.rotateLeft(0)
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r4)
      val r5 = in4.rotateLeft(0)
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r5)
    }

    { // rotateLeft[0]
      val r1 = in0.rotateLeft[0]
      assertTypedSame[EmptyTuple](EmptyTuple, r1)
      val r2 = in1.rotateLeft[0]
      assertTypedSame[I *: EmptyTuple](in1, r2)
      val r3 = in2.rotateLeft[0]
      assertTypedSame[I *: S *: EmptyTuple](in2, r3)
      val r4 = in3.rotateLeft[0]
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r4)
      val r5 = in4.rotateLeft[0]
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r5)
    }

    { // rotateLeft(n % size == 0)
      val r1 = in1.rotateLeft(1)
      assertTypedSame[I *: EmptyTuple](in1, r1)
      val r2 = in1.rotateLeft(2)
      assertTypedSame[I *: EmptyTuple](in1, r2)
      val r3 = in2.rotateLeft(2)
      assertTypedSame[I *: S *: EmptyTuple](in2, r3)
      val r4 = in2.rotateLeft(4)
      assertTypedSame[I *: S *: EmptyTuple](in2, r4)
      val r5 = in3.rotateLeft(3)
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r5)
      val r6 = in3.rotateLeft(6)
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r6)
      val r7 = in4.rotateLeft(4)
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r7)
      val r8 = in4.rotateLeft(8)
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r8)
    }

    { // rotateLeft[N % Size == 0]
    val r1 = in1.rotateLeft[1]
      assertTypedSame[I *: EmptyTuple](in1, r1)
      val r2 = in1.rotateLeft[2]
      assertTypedSame[I *: EmptyTuple](in1, r2)
      val r3 = in2.rotateLeft[2]
      assertTypedSame[I *: S *: EmptyTuple](in2, r3)
      val r4 = in2.rotateLeft[4]
      assertTypedSame[I *: S *: EmptyTuple](in2, r4)
      val r5 = in3.rotateLeft[3]
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r5)
      val r6 = in3.rotateLeft[6]
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r6)
      val r7 = in4.rotateLeft[4]
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r7)
      val r8 = in4.rotateLeft[8]
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r8)
    }

    { // other(n)
      val r1 = in2.rotateLeft(1)
      assertTypedEquals[S *: I *: EmptyTuple]("foo" *: 1 *: EmptyTuple, r1)

      val r2 = in3.rotateLeft(1)
      assertTypedEquals[S *: D *: I *: EmptyTuple]("foo" *: 2.0 *: 1 *: EmptyTuple, r2)

      val r3 = in4.rotateLeft(1)
      assertTypedEquals[S *: D *: C *: I *: EmptyTuple]("foo" *: 2.0 *: 'a' *: 1 *: EmptyTuple, r3)

      val r4 = in4.rotateLeft(2)
      assertTypedEquals[D *: C *: I *: S *: EmptyTuple](2.0 *: 'a' *: 1 *: "foo" *: EmptyTuple, r4)

      val r5 = in4.rotateLeft(3)
      assertTypedEquals[C *: I *: S *: D *: EmptyTuple]('a' *: 1 *: "foo" *: 2.0 *: EmptyTuple, r5)

      val r6 = in4.rotateLeft(5)
      assertTypedEquals[S *: D *: C *: I *: EmptyTuple]("foo" *: 2.0 *: 'a' *: 1 *: EmptyTuple, r6)

      val r7 = in4.rotateLeft(6)
      assertTypedEquals[D *: C *: I *: S *: EmptyTuple](2.0 *: 'a' *: 1 *: "foo" *: EmptyTuple, r7)
    }

    { // other[N]
    val r1 = in2.rotateLeft[1]
      assertTypedEquals[S *: I *: EmptyTuple]("foo" *: 1 *: EmptyTuple, r1)

      val r2 = in3.rotateLeft[1]
      assertTypedEquals[S *: D *: I *: EmptyTuple]("foo" *: 2.0 *: 1 *: EmptyTuple, r2)

      val r3 = in4.rotateLeft[1]
      assertTypedEquals[S *: D *: C *: I *: EmptyTuple]("foo" *: 2.0 *: 'a' *: 1 *: EmptyTuple, r3)

      val r4 = in4.rotateLeft[2]
      assertTypedEquals[D *: C *: I *: S *: EmptyTuple](2.0 *: 'a' *: 1 *: "foo" *: EmptyTuple, r4)

      val r5 = in4.rotateLeft[3]
      assertTypedEquals[C *: I *: S *: D *: EmptyTuple]('a' *: 1 *: "foo" *: 2.0 *: EmptyTuple, r5)

      val r6 = in4.rotateLeft[5]
      assertTypedEquals[S *: D *: C *: I *: EmptyTuple]("foo" *: 2.0 *: 'a' *: 1 *: EmptyTuple, r6)

      val r7 = in4.rotateLeft[6]
      assertTypedEquals[D *: C *: I *: S *: EmptyTuple](2.0 *: 'a' *: 1 *: "foo" *: EmptyTuple, r7)
    }
  }

  @Test
  def testRotateRight: Unit = {
    val in0 = EmptyTuple
    val in1 = 1 *: EmptyTuple
    val in2 = 1 *: "foo" *: EmptyTuple
    val in3 = 1 *: "foo" *: 2.0 *: EmptyTuple
    val in4 = 1 *: "foo" *: 2.0 *: 'a' *: EmptyTuple
    type S = String; type I = Int; type D = Double; type C = Char

    { // rotateRight(0)
      val r1 = in0.rotateRight(0)
      assertTypedSame[EmptyTuple](EmptyTuple, r1)
      val r2 = in1.rotateRight(0)
      assertTypedSame[I *: EmptyTuple](in1, r2)
      val r3 = in2.rotateRight(0)
      assertTypedSame[I *: S *: EmptyTuple](in2, r3)
      val r4 = in3.rotateRight(0)
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r4)
      val r5 = in4.rotateRight(0)
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r5)
    }

    { // rotateRight[0]
      val r1 = in0.rotateRight[0]
      assertTypedSame[EmptyTuple](EmptyTuple, r1)
      val r2 = in1.rotateRight[0]
      assertTypedSame[I *: EmptyTuple](in1, r2)
      val r3 = in2.rotateRight[0]
      assertTypedSame[I *: S *: EmptyTuple](in2, r3)
      val r4 = in3.rotateRight[0]
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r4)
      val r5 = in4.rotateRight[0]
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r5)
    }

    { // rotateRight(n % size == 0)
      val r1 = in1.rotateRight(1)
      assertTypedSame[I *: EmptyTuple](in1, r1)
      val r2 = in1.rotateRight(2)
      assertTypedSame[I *: EmptyTuple](in1, r2)
      val r3 = in2.rotateRight(2)
      assertTypedSame[I *: S *: EmptyTuple](in2, r3)
      val r4 = in2.rotateRight(4)
      assertTypedSame[I *: S *: EmptyTuple](in2, r4)
      val r5 = in3.rotateRight(3)
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r5)
      val r6 = in3.rotateRight(6)
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r6)
      val r7 = in4.rotateRight(4)
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r7)
      val r8 = in4.rotateRight(8)
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r8)
    }

    { // rotateRight[N % Size == 0]
      val r1 = in1.rotateRight[1]
      assertTypedSame[I *: EmptyTuple](in1, r1)
      val r2 = in1.rotateRight[2]
      assertTypedSame[I *: EmptyTuple](in1, r2)
      val r3 = in2.rotateRight[2]
      assertTypedSame[I *: S *: EmptyTuple](in2, r3)
      val r4 = in2.rotateRight[4]
      assertTypedSame[I *: S *: EmptyTuple](in2, r4)
      val r5 = in3.rotateRight[3]
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r5)
      val r6 = in3.rotateRight[6]
      assertTypedSame[I *: S *: D *: EmptyTuple](in3, r6)
      val r7 = in4.rotateRight[4]
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r7)
      val r8 = in4.rotateRight[8]
      assertTypedSame[I *: S *: D *: C *: EmptyTuple](in4, r8)
    }

    { // others(n)
      val r1 = in2.rotateRight(1)
      assertTypedEquals[S *: I *: EmptyTuple]("foo" *: 1 *: EmptyTuple, r1)

      val r2 = in3.rotateRight(1)
      assertTypedEquals[D *: I *: S *: EmptyTuple](2.0 *: 1 *: "foo" *: EmptyTuple, r2)

      val r3 = in4.rotateRight(1)
      assertTypedEquals[C *: I *: S *: D *: EmptyTuple]('a' *: 1 *: "foo" *: 2.0 *: EmptyTuple, r3)

      val r4 = in4.rotateRight(2)
      assertTypedEquals[D *: C *: I *: S *: EmptyTuple](2.0 *: 'a' *: 1 *: "foo" *: EmptyTuple, r4)

      val r5 = in4.rotateRight(3)
      assertTypedEquals[S *: D *: C *: I *: EmptyTuple]("foo" *: 2.0 *: 'a' *: 1 *: EmptyTuple, r5)

      val r6 = in4.rotateRight(5)
      assertTypedEquals[C *: I *: S *: D *: EmptyTuple]('a' *: 1 *: "foo" *: 2.0 *: EmptyTuple, r6)

      val r7 = in4.rotateRight(6)
      assertTypedEquals[D *: C *: I *: S *: EmptyTuple](2.0 *: 'a' *: 1 *: "foo" *: EmptyTuple, r7)
    }

    { // others[N]
      val r1 = in2.rotateRight[1]
      assertTypedEquals[S *: I *: EmptyTuple]("foo" *: 1 *: EmptyTuple, r1)

      val r2 = in3.rotateRight[1]
      assertTypedEquals[D *: I *: S *: EmptyTuple](2.0 *: 1 *: "foo" *: EmptyTuple, r2)

      val r3 = in4.rotateRight[1]
      assertTypedEquals[C *: I *: S *: D *: EmptyTuple]('a' *: 1 *: "foo" *: 2.0 *: EmptyTuple, r3)

      val r4 = in4.rotateRight[2]
      assertTypedEquals[D *: C *: I *: S *: EmptyTuple](2.0 *: 'a' *: 1 *: "foo" *: EmptyTuple, r4)

      val r5 = in4.rotateRight[3]
      assertTypedEquals[S *: D *: C *: I *: EmptyTuple]("foo" *: 2.0 *: 'a' *: 1 *: EmptyTuple, r5)

      val r6 = in4.rotateRight[5]
      assertTypedEquals[C *: I *: S *: D *: EmptyTuple]('a' *: 1 *: "foo" *: 2.0 *: EmptyTuple, r6)

      val r7 = in4.rotateRight[6]
      assertTypedEquals[D *: C *: I *: S *: EmptyTuple](2.0 *: 'a' *: 1 *: "foo" *: EmptyTuple, r7)
    }
  }

  object smear extends Poly2 {
    given caseIntInt: Case.Aux[Int, Int, Int] = at((x: Int, y: Int) => x + y)
    given caseStringInt: Case.Aux[String, Int, Int] = at((x: String, y: Int) => x.toInt + y)
    given caseIntString: Case.Aux[Int, String, Int] = at((x: Int, y: String) => x + y.toInt)
  }

  @Test
  def testScanLeft: Unit = {
    val in = 1 *: "2" *: EmptyTuple
    val out = in.scanLeft(1)(smear)

    typed[Int *: Int *: Int *: EmptyTuple](out)
    assertEquals(1 *: 2 *: 4 *: EmptyTuple, out)
  }

  @Test
  def testScanRight: Unit = {
    val in = 1 *: "2" *: EmptyTuple
    val out = in.scanRight(1)(smear)

    typed[Int *: Int *: Int *: EmptyTuple](out)
    assertEquals(4 *: 3 *: 1 *: EmptyTuple, out)
  }

  @Test
  def testFill: Unit = {
    {
      val empty = Tuple.fill(0)(true)
      typed[0](empty.length)
    }

    {
      val empty = Tuple.fill[Boolean](0)(true)
      typed[0](empty.length)
    }

    {
      val single = Tuple.fill(1)(None)
      typed[1](single.length)
      typed[None.type](single.head)
      assertEquals(None, single.head)
    }

    {
      val single = Tuple.fill[None.type](1)(None)
      typed[1](single.length)
      typed[None.type](single.head)
      assertEquals(None, single.head)
    }

    {
      val three = Tuple.fill(3)(m2i)
      typed[3](three.length)
      typed[M2[Int, Unit]](three(0))
      typed[M2[Int, Unit]](three(1))
      typed[M2[Int, Unit]](three(2))
      assertEquals(m2i, three(0))
      assertEquals(m2i, three(1))
      assertEquals(m2i, three(2))
    }

    {
      val three = Tuple.fill[M2[Int, Unit]](3)(m2i)
      typed[3](three.length)
      typed[M2[Int, Unit]](three(0))
      typed[M2[Int, Unit]](three(1))
      typed[M2[Int, Unit]](three(2))
      assertEquals(m2i, three(0))
      assertEquals(m2i, three(1))
      assertEquals(m2i, three(2))
    }

    {
      val empty = Tuple.fill(0, 0)(true)
      typed[0](empty.length)
    }

    {
      val empty = Tuple.fill[Boolean](0, 0)(true)
      typed[0](empty.length)
    }

    {
      val empty = Tuple.fill(2, 0)(true)
      typed[2](empty.length)
      typed[0](empty(0).length)
      typed[0](empty(1).length)
    }

    {
      val empty = Tuple.fill[Boolean](2, 0)(true)
      typed[2](empty.length)
      typed[0](empty(0).length)
      typed[0](empty(1).length)
    }

    {
      val empty = Tuple.fill(0, 2)(true)
      typed[0](empty.length)
    }

    {
      val empty = Tuple.fill[Boolean](0, 2)(true)
      typed[0](empty.length)
    }

    {
      val oneByTwo = Tuple.fill(1, 2)(None)
      typed[1](oneByTwo.length)
      typed[2](oneByTwo.head.length)
      typed[None.type](oneByTwo.head(0))
      typed[None.type](oneByTwo.head(1))
      assertEquals(None, oneByTwo.head(0))
      assertEquals(None, oneByTwo.head(1))
    }

    {
      val oneByTwo = Tuple.fill[None.type](1, 2)(None)
      typed[1](oneByTwo.length)
      typed[2](oneByTwo.head.length)
      typed[None.type](oneByTwo.head(0))
      typed[None.type](oneByTwo.head(1))
      assertEquals(None, oneByTwo.head(0))
      assertEquals(None, oneByTwo.head(1))
    }

    {
      val twoByThree = Tuple.fill(2, 3)(None)
      typed[2](twoByThree.length)
      typed[3](twoByThree(0).length)
      typed[3](twoByThree(1).length)
      typed[None.type](twoByThree.at[0].at[0])
      typed[None.type](twoByThree.at[0].at[1])
      typed[None.type](twoByThree.at[0].at[2])
      typed[None.type](twoByThree.at[1].at[0])
      typed[None.type](twoByThree.at[1].at[1])
      typed[None.type](twoByThree.at[1].at[2])
      assertEquals(None, twoByThree.at[0].at[0])
      assertEquals(None, twoByThree.at[0].at[1])
      assertEquals(None, twoByThree.at[0].at[2])
      assertEquals(None, twoByThree.at[1].at[0])
      assertEquals(None, twoByThree.at[1].at[1])
      assertEquals(None, twoByThree.at[1].at[2])
    }

    {
      val twoByThree = Tuple.fill[None.type](2, 3)(None)
      typed[2](twoByThree.length)
      typed[3](twoByThree(0).length)
      typed[3](twoByThree(1).length)
      typed[None.type](twoByThree.at[0].at[0])
      typed[None.type](twoByThree.at[0].at[1])
      typed[None.type](twoByThree.at[0].at[2])
      typed[None.type](twoByThree.at[1].at[0])
      typed[None.type](twoByThree.at[1].at[1])
      typed[None.type](twoByThree.at[1].at[2])
      assertEquals(None, twoByThree.at[0].at[0])
      assertEquals(None, twoByThree.at[0].at[1])
      assertEquals(None, twoByThree.at[0].at[2])
      assertEquals(None, twoByThree.at[1].at[0])
      assertEquals(None, twoByThree.at[1].at[1])
      assertEquals(None, twoByThree.at[1].at[2])
    }
  }

  @Test
  def testPolyFill = {
    object zero extends Poly0 {
      given zeroInt: Case.Aux[Int] = at[Int](0)
    }

    implicit val emptyString = zero.at[String]("")

    val out = Tuple.fillWith[Int *: String *: Int *: EmptyTuple](zero)
    assertEquals(out, 0 *: "" *: 0 *: EmptyTuple)
  }

  @Test
  def testPatch: Unit = {
    val basehl = 1 *: 2 *: "three" *: EmptyTuple

    { //patch an empty hlist
      val out = EmptyTuple.patch(0, basehl, 0)
      val out2 = EmptyTuple.patch[0,0](basehl)

      typed[Int *: Int *: String *: EmptyTuple](out)
      assertEquals(out, basehl)
      assertTypedEquals[Int *: Int *: String *: EmptyTuple](out, out2)
    }

    { //single patch w/ nothing removed
      val out = basehl.patch(1, 4 *: EmptyTuple, 0)
      val out2 = basehl.patch[1,0](4 *: EmptyTuple)

      typed[Int *: Int *: Int *: String *: EmptyTuple](out)
      assertEquals(1 *: 4 *: 2 *: "three" *: EmptyTuple, out)
      assertTypedEquals[Int *: Int *: Int *: String *: EmptyTuple](out, out2)
    }

    { //single patch w/ 2 elements removed
      val out = basehl.patch(1, 3 *: EmptyTuple, 2)
      val out2 = basehl.patch[1,2](3 *: EmptyTuple)

      typed[Int *: Int *: EmptyTuple](out)
      assertEquals(1 *: 3 *: EmptyTuple, out)
      assertTypedEquals[Int *: Int *: EmptyTuple](out, out2)
    }

    { //essentially append
      val p = 4 *: 5 *: "six" *: EmptyTuple
      val out = basehl.patch(3, p, 0)
      val out2 = basehl.patch[3,0](p)

      typed[Int *: Int *: String *: Int *: Int *: String *: EmptyTuple](out)
      assertEquals(1 *: 2 *: "three" *: 4 *: 5 *: "six" *: EmptyTuple, out)
      assertTypedEquals[Int *: Int *: String *: Int *: Int *: String *: EmptyTuple](out, out2)
    }

    { //several patched w/ everything from original removed
      val sub = 4 *: "five" *: "six" *: EmptyTuple
      val out = basehl.patch(0, sub, 3)
      val out2 = basehl.patch[0,3](sub)

      typed[Int *: String *: String *: EmptyTuple](out)
      assertEquals(sub, out)
      assertTypedEquals[Int *: String *: String *: EmptyTuple](out, out2)
    }
  }

  // @Test // TODO
  // def testToEither: Unit = {
  //   type PISB = Int *: String *: Boolean *: EmptyTuple
  //   type CISBa = Int :+: String :+: Boolean :+: CNil
  //   type CISBb = the.`ToCoproduct[PISB]`.Out
  //   implicitly[CISBa =:= CISBb]
  // }

  // @Test
  // def testToSum: Unit = {
  //   type PISB = Int *: String *: Boolean *: EmptyTuple
  //   type CISBa = Int :+: String :+: Boolean :+: CNil
  //   type SISBa = the.`ToSum[PISB]`.Out
  //   implicitly[CISBa =:= SISBa]

  //   type PIISSB = Int *: Int *: String *: String *: Boolean *: EmptyTuple
  //   type SISBb = the.`ToSum[PIISSB]`.Out
  //   implicitly[CISBa =:= SISBb]
  // }

  @Test
  def testTupleTypeSelector: Unit = {
    import syntax.singleton._

    typed[Tuple.` `.T](EmptyTuple)

    typed[Tuple.`Int`.T](23 *: EmptyTuple)

    typed[Tuple.`Int, String`.T](23 *: "foo" *: EmptyTuple)

    typed[Tuple.`Int, String, Boolean`.T](23 *: "foo" *: true *: EmptyTuple)

    // Literal types

    typed[Tuple.`2`.T](2.narrow *: EmptyTuple)

    typed[Tuple.`2, "a", true`.T](2.narrow *: "a".narrow *: true.narrow *: EmptyTuple)

    illTyped(""" typed[Tuple.`2`.T](3.narrow *: EmptyTuple) """)

    // Mix of standard and literal types

    typed[Tuple.`2, String, true`.T](2.narrow *: "a" *: true.narrow *: EmptyTuple)
  }

  // object Foo extends ProductArgs {
  //   def applyProduct[L <: Tuple](args: L): L = args
  // }

  // @Test
  // def testProductArgs: Unit = {
  //   val l = Foo(23, "foo", true)
  //   typed[Int *: String *: Boolean *: EmptyTuple](l)

  //   val v1 = l.head
  //   typed[Int](v1)
  //   assertEquals(23, v1)

  //   val v2 = l.tail.head
  //   typed[String](v2)
  //   assertEquals("foo", v2)

  //   val v3 = l.tail.tail.head
  //   typed[Boolean](v3)
  //   assertEquals(true, v3)

  //   val v4 = l.tail.tail.tail
  //   typed[EmptyTuple](v4)

  //   illTyped("""
  //     r.tail.tail.tail.head
  //   """)
  // }

  // object SFoo extends SingletonProductArgs {
  //   def applyProduct[L <: Tuple](args: L): L = args
  // }

  // case class Quux(i: Int, s: String, b: Boolean)

  // object selectAll extends SingletonProductArgs {
  //   class Apply[K <: Tuple] {
  //     def from[T, R <: Tuple, S <: Tuple, Out](t: T)
  //       (implicit
  //         gen: LabelledGeneric.Aux[T, R],
  //         sel: SelectAll.Aux[R, K, S],
  //         tp: Tupler.Aux[S, Out]
  //       ): Out =
  //       tp(sel(gen.to(t)))
  //   }

  //   def applyProduct[K <: Tuple](keys: K) = new Apply[K]
  // }

  // trait NonSingletonEmptyTupleTC[T]
  // object NonSingletonEmptyTupleTC {
  //   def apply[T](t: T)(implicit i: NonSingletonEmptyTupleTC[T]): NonSingletonEmptyTupleTC[T] = i

  //   implicit val nsEmptyTupleTC: NonSingletonEmptyTupleTC[EmptyTuple] = new NonSingletonEmptyTupleTC[EmptyTuple] {}
  // }

  // @Test
  // def testSingletonProductArgs: Unit = {
  //   object Obj

  //   val l = SFoo(23, "foo", Symbol("bar"), Obj, true)
  //   typed[Witness.`23`.T *: Witness.`"foo"`.T *: Witness.`'bar`.T *: Obj.type *: Witness.`true`.T *: EmptyTuple](l)

  //   // Annotations on the LHS here and subsequently, otherwise scalac will
  //   // widen the RHS to a non-singleton type.
  //   val v1: Witness.`23`.T = l.head
  //   assertEquals(23, v1)

  //   val v2: Witness.`"foo"`.T = l.tail.head
  //   assertEquals("foo", v2)

  //   val v3: Witness.`'bar`.T = l.tail.tail.head
  //   assertEquals(Symbol("bar"), v3)

  //   val v4: Obj.type = l.tail.tail.tail.head
  //   assertEquals(Obj, v4)

  //   val v5: Witness.`true`.T = l.tail.tail.tail.tail.head
  //   assertEquals(true, v5)

  //   val v6 = l.tail.tail.tail.tail.tail
  //   typed[EmptyTuple](v6)

  //   illTyped("""
  //     r.tail.tail.tail.tail.tail.tail.head
  //   """)

  //   // Verify that we infer EmptyTuple rather than EmptyTuple.type at the end
  //   NonSingletonEmptyTupleTC(SFoo(23).tail)
  //   NonSingletonEmptyTupleTC(SFoo())

  //   val quux = Quux(23, "foo", true)
  //   val ib = selectAll(Symbol("i"), Symbol("b")).from(quux)
  //   typed[(Int, Boolean)](ib)
  //   assertEquals((23, true), ib)
  // }

  // object Bar extends FromProductArgs {
  //   def sumLabel(k: String, i1: Int, i2: Int) = (k, i1 + i2)
  //   def sumImplicitLabel(k: String, i1: Int)(implicit i2: Int) = (k, i1 + i2)
  //   def sumMultipleParamListLabel(k: String, i1: Int)(i2: Int) = (k, i1 + i2)
  // }

  // @Test
  // def testFromProductArgs: Unit = {
  //   val p = "foo" *: 1 *: 3 *: EmptyTuple

  //   val v1 = Bar.sumLabelProduct(p)
  //   typed[(String, Int)](v1)
  //   assertEquals(("foo", 4), v1)

  //   val p2 = "bar" *: 1 *: 2 *: EmptyTuple
  //   val v2 = Bar.sumMultipleParamListLabelProduct(p2)
  //   typed[(String, Int)](v2)
  //   assertEquals(("bar", 3), v2)

  //   illTyped("""
  //     Bar.sumImplicitLabelProduct("foo" *: 1 *: 3 *: EmptyTuple)
  //   """)

  //   implicit val i2 = 7
  //   val v3 = Bar.sumImplicitLabelProduct("foo" *: 1 *: EmptyTuple)
  //   typed[(String, Int)](v3)
  //   assertEquals(("foo", 8), v3)

  //   illTyped("""
  //     Bar.sumLabelProduct("foo" *: "bar" *: 1 *: 2 *: EmptyTuple)
  //   """)

  //   illTyped("""
  //     Bar.sumMultipleParamListLabelProduct("foo" *: "1" *: 2 *: 3 *: EmptyTuple)
  //   """)
  // }

  @Test
  def selectAllTest: Unit ={
    import shapeless._, record._ , ops.hlist.SelectAll

    //is there any way to do it without runtime overhead?
    class TypeCaptured[T](val value: T) {
      type _type = T
    }

    def getFieldsByTypesOfSuper[Sub <: Tuple, Super <: Tuple](l: Sub)(implicit sa: SelectAll[Sub, Super]) = sa(l)

    val hsuper = new TypeCaptured("2":: true *: EmptyTuple)
    val hsub = new TypeCaptured(1 *: "2":: true *: EmptyTuple)

    //testing with plain Tuple
    assertTypedEquals[hsuper._type](hsuper.value, getFieldsByTypesOfSuper[hsub._type, hsuper._type](hsub.value))

    val rsuper = new TypeCaptured(Record(b = true, c = "blah"))
    val rsub =  new TypeCaptured(Record(a = 1, b = true, c = "blah"))

    //testing with Record
    assertTypedEquals[rsuper._type](rsuper.value, getFieldsByTypesOfSuper[rsub._type, rsuper._type](rsub.value))

  }

  // object FooNat extends NatProductArgs {
  //   def applyNatProduct[L <: Tuple](args: L): L = args
  // }
  // object FooNatTypeParams extends NatProductArgs {
  //   def applyNatProduct[L <: Tuple](implicit len: Length[L]) = len()
  // }

  // @Test
  // def testNatProductArgs: Unit = {
  //   val l = FooNat(1, 2, 3)
  //   typed[1 *: _2 *: _3 *: EmptyTuple](l)

  //   val v1 = l.head
  //   typed[1](v1)
  //   assertEquals(1, v1)

  //   val v2 = l.tail.head
  //   typed[2](v2)
  //   assertEquals(2, v2)

  //   val v3 = l.tail.tail.head
  //   typed[3](v3)
  //   assertEquals(3, v3)

  //   val v4 = l.tail.tail.tail
  //   typed[EmptyTuple](v4)

  //   illTyped("""
  //     r.tail.tail.tail.head
  //            """)
  //   val res = FooNatTypeParams(1,2,3,4)
  //   assertEquals(4,res)
  // }

  // implicit class Interpolator(val sc: StringContext) {
  //   class Args extends ProductArgs {
  //     def applyProduct[L <: Tuple](l: L): L = l
  //   }

  //   val hlist: Args = new Args
  // }

  // @Test
  // def testStringInterpolator: Unit = {
  //   val (i, s, b) = (23, "foo", true)
  //   val l = hlist"Int: $i, String: $s, Boolean: $b"
  //   typed[Int *: String *: Boolean *: EmptyTuple](l)

  //   val v1 = l.head
  //   typed[Int](v1)
  //   assertEquals(23, v1)

  //   val v2 = l.tail.head
  //   typed[String](v2)
  //   assertEquals("foo", v2)

  //   val v3 = l.tail.tail.head
  //   typed[Boolean](v3)
  //   assertEquals(true, v3)

  //   val v4 = l.tail.tail.tail
  //   typed[EmptyTuple](v4)

  //   illTyped("""
  //     r.tail.tail.tail.head
  //   """)
  // }

  @Test
  def testCollectFirst: Unit = {
    object Foo extends Poly1{
      given iinst: Case.Aux[Int, Int] = at[Int]{ _ + 1 }
    }
    val hlist1 = "foo" *: 2.0 *: 1 *: EmptyTuple
    assertTypedEquals[Int](hlist1.collectFirst(Foo), 2)

    val hlist2 = "foo" *: 2.0 *: EmptyTuple
    illTyped("""hlist2.collectFirst(Foo)""")
  }

  @Test
  def testGrouper: Unit = {
    object toInt extends Poly1 {
      given default[N <: Int]: Case.Aux[N, Int] = at[N](identity)
    }
    def range[R <: Tuple](a: Int, b: Int)(
      using range: Range.Aux[a.type, b.type, R],
      mapper: Mapper[toInt.type, R]
    ) = mapper(range())

    // group EmptyTuple
    assertEquals(EmptyTuple: EmptyTuple, (EmptyTuple: EmptyTuple) group(2, 1))
    // group a Tuple of 4 items into 2 (4/2) tuples of 2 items
    assertEquals(
      (0, 1) ::(2, 3) *: EmptyTuple,
      range(0, 4) group(2, 2)
    )

    // group a Tuple of 5 items into 2 (5/2) tuples of 2 items
    // the last item does not make a complete partition and is dropped.
    assertEquals(
      (0, 1) ::(2, 3) *: EmptyTuple,
      range(0, 5) group(2, 2)
    )

    // uses the step to select the starting point for each partition
    assertEquals(
      (0, 1) ::(4, 5) *: EmptyTuple,
      range(0, 6) group(2, 4)
    )

    // if the step is smaller than the partition size, items will be reused
    assertEquals(
      (0, 1) ::(1, 2) ::(2, 3) *: EmptyTuple,
      range(0, 4) group(2, 1)
    )

    // when there are not enough items to fill the last partition, a pad can be supplied.
    assertEquals(
      (0, 1) ::(2, 3) ::(4, 'a') *: EmptyTuple,
      range(0, 5) group(2, 2, 'a' *: EmptyTuple)
    )

    // but only as many pad elements are used as necessary to fill the final partition.
    assertEquals(
      (0, 1) ::(2, 3) ::(4, 'a') *: EmptyTuple,
      range(0, 5) group(2, 2, 'a' *: 'b' *: 'c' *: EmptyTuple)
    )

  }

  @Test
  def testLiftAll: Unit = {
    trait F[A]
    implicit object FInt extends F[Int]
    implicit object FString extends F[String]

    assertEquals(EmptyTuple, implicitly[LiftAll[F, EmptyTuple]].instances)
    assertEquals(FInt *: EmptyTuple, implicitly[LiftAll[F, Int *: EmptyTuple]].instances)
    assertEquals(FString *: FInt *: EmptyTuple, implicitly[LiftAll[F, String *: Int *: EmptyTuple]].instances)
    illTyped("implicitly[LiftAll[F, Long *: String *: Int *: EmptyTuple]]")

    assertEquals(FInt *: EmptyTuple, LiftAll[F](1 *: EmptyTuple).instances)
  }

  @Test
  def testPadTo: Unit = {
    val p1 = (1 *: "a" *: EmptyTuple).padTo(3, 0)
    assertTypedEquals[Int *: String *: Int *: EmptyTuple](1 *: "a" *: 0 *: EmptyTuple, p1)

    val p2 = (1 *: "a" *: EmptyTuple).padTo(2, 0)
    assertTypedEquals[Int *: String *: EmptyTuple](1 *: "a" *: EmptyTuple, p2)

    val p3 = (EmptyTuple: EmptyTuple).padTo(2, "a")
    assertTypedEquals[String *: String *: EmptyTuple]("a" *: "a" *: EmptyTuple, p3)

    val p4 = (EmptyTuple: EmptyTuple).padTo(0, "a")
    assertTypedEquals[EmptyTuple](EmptyTuple, p4)

    illTyped(""" (1 *: "a" *: EmptyTuple).padTo(1, 0) """)
  }

  @Test
  def testSlice: Unit = {
    val r1 = (1 *: "a" *: 3 *: EmptyTuple).slice(0, 2)
    assertTypedEquals[Int *: String *: EmptyTuple](1 *: "a" *: EmptyTuple, r1)

    val r2 = (1 *: "a" *: 3 *: EmptyTuple).slice(1, 2)
    assertTypedEquals[String *: EmptyTuple]("a" *: EmptyTuple, r2)

    val r3 = (1 *: "a" *: 3 *: EmptyTuple).slice(2, 3)
    assertTypedEquals[Int *: EmptyTuple](3 *: EmptyTuple, r3)

    val r4 = (EmptyTuple: EmptyTuple).slice(0, 0)
    assertTypedEquals[EmptyTuple](EmptyTuple, r4)

    illTyped(""" (1 *: "a" *: 3 *: EmptyTuple).slice(0, 4) """)
    illTyped(""" (1 *: "a" *: 3 *: EmptyTuple).slice(1, 0) """)
  }

  // @Test
  // def testToSizedTuple: Unit = {
  //   val ns = List(1,2,3,4)
  //   assertTypedEquals[Option[III]](None, ns.toSizedTuple(3))
  //   assertTypedEquals[Option[IIII]](Some(1 *: 2 *: 3 *: 4 *: EmptyTuple), ns.toSizedTuple(4))
  // }

  @Test
  def testModifierAt: Unit = {
    // first element
    assertEquals((1, 42 *: 2 *: 3 *: EmptyTuple), (1 *: 2 *: 3 *: EmptyTuple).updateAtWith(0)(_ => 42))

    //last element
    assertEquals((3, 1 *: 2 *: 42 *: EmptyTuple), (1 *: 2 *: 3 *: EmptyTuple).updateAtWith(2)(_ => 42))

    //different type
    assertEquals((3, 1 *: 2 *: 42.0 *: EmptyTuple), (1 *: 2 *: 3 *: EmptyTuple).updateAtWith(2)(_ => 42.0))
  }

  @Test
  def testReify: Unit = {
    import syntax.singleton._

    assertTypedEquals(EmptyTuple, Reify[EmptyTuple].apply())

    val s1 = Tuple.`'a`
    assertTypedEquals(Symbol("a").narrow *: EmptyTuple, Reify[s1.T].apply())

    val s2 = Tuple.`'a, 1, "b", true`
    assertTypedEquals(Symbol("a").narrow *: 1.narrow *: "b".narrow *: true.narrow *: EmptyTuple, Reify[s2.T].apply())

    illTyped(""" Reify[String *: Int *: EmptyTuple] """)
    illTyped(""" Reify[String *: Tuple.`'a, 1, "b", true`.T] """)
  }

  @Test
  def testCombinations: Unit = {
    type I = Int; type S = String

    val r1 = (1 *: "2" *: 3 *: 4 *: EmptyTuple).combinations(2)
    assertTypedEquals[
      (I *: S *: EmptyTuple) ::
      (I *: I *: EmptyTuple) ::
      (I *: I *: EmptyTuple) ::
      (S *: I *: EmptyTuple) ::
      (S *: I *: EmptyTuple) ::
      (I *: I *: EmptyTuple) *: EmptyTuple
    ](
      (1 *: "2" *: EmptyTuple) ::
      (1 *: 3 *: EmptyTuple) ::
      (1 *: 4 *: EmptyTuple) ::
      ("2" *: 3 *: EmptyTuple) ::
      ("2" *: 4 *: EmptyTuple) ::
      (3 *: 4 *: EmptyTuple) *: EmptyTuple, r1)

    val r2 = (1 *: "2" *: 3 *: 4 *: EmptyTuple).combinations(3)
    assertTypedEquals[
      (I *: S *: I *: EmptyTuple) ::
      (I *: S *: I *: EmptyTuple) ::
      (I *: I *: I *: EmptyTuple) ::
      (S *: I *: I *: EmptyTuple) *: EmptyTuple
    ](
      (1 *: "2" *: 3 *: EmptyTuple) ::
      (1 *: "2" *: 4 *: EmptyTuple) ::
      (1 *: 3 *: 4 *: EmptyTuple) ::
      ("2" *: 3 *: 4 *: EmptyTuple) *: EmptyTuple, r2)

    val r3 = (1 *: "2" *: 3 *: 4 *: EmptyTuple).combinations(4)
    assertTypedEquals[
      (I *: S *: I *: I *: EmptyTuple) *: EmptyTuple
    ](
      (1 *: "2" *: 3 *: 4 *: EmptyTuple) *: EmptyTuple, r3)

    val r4 = (1 *: "2" *: 3 *: 4 *: EmptyTuple).combinations(5)
    assertTypedEquals[EmptyTuple](EmptyTuple, r4)

    val r5 = (1 *: "2" *: 3 *: 4 *: EmptyTuple).combinations(0)
    assertTypedEquals[EmptyTuple *: EmptyTuple](EmptyTuple *: EmptyTuple, r5)
  }

  @Test
  def testIsNonEmptyTuple = assertTypedEquals[Int *: EmptyTuple](23 *: EmptyTuple, IsNonEmptyTuple[Int *: EmptyTuple].cons(23, EmptyTuple))

  @Test
  def testToProduct = {
    val isbd = 2 *: "abc" *: true *: 3.0 *: EmptyTuple
    val p = (2, ("abc", (true, (3.0, ()))))

    import syntax.std.tuple._
    assertEquals(isbd.toProduct, p)
    assertEquals(p.toTuple, isbd)
    assertEquals(isbd.toProduct.toTuple, isbd)
    assertEquals(p.toTuple.toProduct, p)
    assertEquals((), (EmptyTuple: EmptyTuple).toProduct)
    assertEquals(EmptyTuple, ().toTuple)
  }

  @Test
  def testAuxImplicits: Unit = {
    the[SplitRight.Aux[String *: Int *: Boolean *: EmptyTuple, Int, String *: Int *: EmptyTuple, Boolean *: EmptyTuple]]
    the[Grouper.Aux[Int *: String *: Boolean *: EmptyTuple, _2, _1, (Int, String) *: (String, Boolean) *: EmptyTuple]]
    the[PaddedGrouper.Aux[Int *: String *: Boolean *: EmptyTuple, _2, _2, Long *: EmptyTuple, (Int, String) *: (Boolean, Long) *: EmptyTuple]]
  }
}
