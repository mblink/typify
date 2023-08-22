/*
 * Copyright (c) 2011-14 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package typify
package record

import munit.FunSuite
import typify.tagged.{TranslucentTag => @@, translucentTag}
import typify.test.*
import typify.testutil.*
import typify.tuple.*

class RecordTests extends FunSuite {
  object intField1 extends FieldOf[Int]
  object intField2 extends FieldOf[Int]
  object stringField1 extends FieldOf[String]
  object stringField2 extends FieldOf[String]
  object boolField1 extends FieldOf[Boolean]
  object boolField2 extends FieldOf[Boolean]
  object doubleField1 extends FieldOf[Double]
  object doubleField2 extends FieldOf[Double]

  case class Bar(a: Int, b: String)

  test("Get") {
    val r1 =
      (intField1    ->>    23) *:
      (stringField1 ->> "foo") *:
      (boolField1   ->>  true) *:
      (doubleField1 ->>   2.0) *:
      EmptyTuple

    val v1 = r1.get(intField1)
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r1.get(stringField1)
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r1.get(boolField1)
    typed[Boolean](v3)
    assertEquals(true, v3)

    val v4 = r1.get(doubleField1)
    typed[Double](v4)
    assertEqualsDouble(2.0, v4, Double.MinPositiveValue)
  }

  test("GetLiterals") {
    val r1 =
      ("intField1"    ->>    23) *:
      ("stringField1" ->> "foo") *:
      ("boolField1"   ->>  true) *:
      ("doubleField1" ->>   2.0) *:
      EmptyTuple

    val v1 = r1.get("intField1")
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r1.get("stringField1")
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r1.get("boolField1")
    typed[Boolean](v3)
    assertEquals(true, v3)

    val v4 = r1.get("doubleField1")
    typed[Double](v4)
    assertEqualsDouble(2.0, v4, Double.MinPositiveValue)
  }

  test("FieldAt") {
    val r1 =
      (stringField1 ->> "toto") *:
      (boolField1   ->> true)   *:
      EmptyTuple

    val v1 = r1.fieldAt(stringField1)
    val v2 = r1.fieldAt(boolField1)
    typed[stringField1.F](v1)
    typed[boolField1.F](v2)
    assertEquals("toto", v1: String)
    assertEquals(true, v2: Boolean)
    assertEquals(r1, v1 *: v2 *: EmptyTuple)
  }

  test("At") {
    val r1 =
      (intField1    ->>    23) *:
      (stringField1 ->> "foo") *:
      (boolField1   ->>  true) *:
      (doubleField1 ->>   2.0) *:
      EmptyTuple

    val v1 = r1.at[0]
    typed[Int](v1)
    assertEquals(23, v1: Int)

    val v2 = r1.at[1]
    typed[String](v2)
    assertEquals("foo", v2: String)

    val v3 = r1.at[2]
    typed[Boolean](v3)
    assertEquals(true, v3: Boolean)

    val v4 = r1.at[3]
    typed[Double](v4)
    assertEqualsDouble(2.0, v4: Double, Double.MinPositiveValue)
  }

  test("FromMap") {
    type T1 = ("stringVal" ->> String) *: ("intVal" ->> Int) *: ("boolVal" ->> Boolean) *: EmptyTuple

    val in = Map("intVal" -> 4, "stringVal" -> "Blarr", "boolVal" -> true)

    val recOption = in.toRecord[T1]

    assert(recOption.isDefined)

    val rec: T1 = recOption.get

    typed[T1](rec)

    assert(rec("stringVal") == "Blarr", "stringVal mismatch")
    assert(rec("intVal") == 4, "int val mismatch")
    assert(rec("boolVal"), "Boolean val match")

    val in2 = Map("intVal" -> 4, "stringVal" -> "Blarr")

    val recEither2 = in2.toRecord[T1]

    assert(recEither2.isEmpty)
  }

  test("FromMap2") {
    type T = intField1.F *: stringField1.F *: boolField1.F *: doubleField1.F *: EmptyTuple

    val in = Map(intField1 -> 4, stringField1 -> "Blarr", boolField1 -> true, doubleField1 -> 5.0)

    val recOption = in.toRecord[T]

    assert(recOption.isDefined)

    val rec: T = recOption.get

    typed[T](rec)

    assert(rec(intField1) == 4)
    assert(rec(stringField1) == "Blarr")
    assert(rec(doubleField1) == 5.0)
  }


  test("AtLiterals") {
    val r1 =
      ("intField1"    ->>    23) *:
      ("stringField1" ->> "foo") *:
      ("boolField1"   ->>  true) *:
      ("doubleField1" ->>   2.0) *:
      EmptyTuple

    val v1 = r1.at[0]
    typed[Int](v1)
    assertEquals(23, v1: Int)

    val v2 = r1.at[1]
    typed[String](v2)
    assertEquals("foo", v2: String)

    val v3 = r1.at[2]
    typed[Boolean](v3)
    assertEquals(true, v3: Boolean)

    val v4 = r1.at[3]
    typed[Double](v4)
    assertEqualsDouble(2.0, v4: Double, Double.MinPositiveValue)
  }

  test("Update") {
    val r1 =
      (intField1    ->>    23) *:
      (stringField1 ->> "foo") *:
      (boolField1   ->>  true) *:
      (doubleField1 ->>   2.0) *:
      EmptyTuple

    val r2 = r1.updated(intField1, 7)
    val v1 = r2.get(intField1)
    typed[Int](v1)
    assertEquals(7, v1)

    val r3 = r1.updated(stringField1, "wibble")
    val v2 = r3.get(stringField1)
    typed[String](v2)
    assertEquals("wibble", v2)

    val r4 = r1.updated(boolField1, false)
    val v3 = r4.get(boolField1)
    typed[Boolean](v3)
    assertEquals(false, v3)

    val r5 = r1.updated(doubleField1, 1.0)
    val v4 = r5.get(doubleField1)
    typed[Double](v4)
    assertEqualsDouble(1.0, v4, Double.MinPositiveValue)

    val r6 = EmptyTuple

    val r7 = r6.updated(boolField2, false)
    val v5 = r7.get(boolField2)
    typed[Boolean](v5)
    assertEquals(false, v5)

    val r8 = r7.updated(doubleField2, 3.0)
    val v6 = r8.get(doubleField2)
    typed[Double](v6)
    assertEqualsDouble(3.0, v6, Double.MinPositiveValue)
  }

  test("UpdateLiteral") {
    val r1 =
      ("intField1"    ->>    23) *:
      ("stringField1" ->> "foo") *:
      ("boolField1"   ->>  true) *:
      ("doubleField1" ->>   2.0) *:
      EmptyTuple

    val r2 = r1.updated("intField1", 7)
    val v1 = r2.get("intField1")
    typed[Int](v1)
    assertEquals(7, v1)

    val r3 = r1.updated("stringField1", "wibble")
    val v2 = r3.get("stringField1")
    typed[String](v2)
    assertEquals("wibble", v2)

    val r4 = r1.updated("boolField1", false)
    val v3 = r4.get("boolField1")
    typed[Boolean](v3)
    assertEquals(false, v3)

    val r5 = r1.updated("doubleField1", 1.0)
    val v4 = r5.get("doubleField1")
    typed[Double](v4)
    assertEqualsDouble(1.0, v4, Double.MinPositiveValue)

    val r6 = EmptyTuple

    val r7 = r6.updated("boolField2", false)
    val v5 = r7.get("boolField2")
    typed[Boolean](v5)
    assertEquals(false, v5)

    val r8 = r7.updated("doubleField2", 3.0)
    val v6 = r8.get("doubleField2")
    typed[Double](v6)
    assertEqualsDouble(3.0, v6, Double.MinPositiveValue)
  }

  test("Merge") {
    val r1 = ("a" ->> 23) *: ("b" ->> "foo") *: ("c" ->> true) *: EmptyTuple
    val r2 = ("c" ->> false) *: ("a" ->> 13) *: EmptyTuple
    val rExp = ("a" ->> 13) *: ("b" ->> "foo") *: ("c" ->> false) *: EmptyTuple

    val rm = r1.merge(r2)
    typed[("a" ->> Int) *: ("b" ->> String) *: ("c" ->> Boolean) *: EmptyTuple](rm)
    assertEquals(rExp, rm)
  }

  // test("DeepMerge") {
  //   val r3 = ("d" ->> (("x" ->> "X1") *: ("m" ->> "M") *: EmptyTuple)) *: ("e" ->> true) *: ("x" ->> "X") *: EmptyTuple
  //   val r4 = ("d" ->> "D") *: ("e" ->> false) *: ("x" ->> 2) *: ("m" ->> 6) *: EmptyTuple
  //   val r5 = ("d" ->> "A") *: ("d" ->> "B") *: ("d" ->> "C") *: EmptyTuple

  //   assertTypedEquals(r4.merge(r3))(r4.deepMerge(r3))
  //   assertTypedEquals(r3.merge(r4))(r3.deepMerge(r4))
  //   assertTypedEquals(r3.merge(r5))(r3.deepMerge(r5))
  //   assertTypedEquals(r5.merge(r3))(r5.deepMerge(r3))

  //   //nested
  //   val inner1 = ("d" ->> "D") *: ("e" ->> false) *: EmptyTuple
  //   val inner2 = ("d" ->> 3) *: ("m" ->> 2D) *: EmptyTuple
  //   val outer1 = ("d" ->> 10) *: ("e" ->> inner1) *: ("x" ->> "boo") *: EmptyTuple
  //   val outer2 = ("x" ->> "foo") *: ("d" ->> -1) *: ("e" ->> inner2) *: EmptyTuple

  //   val innerMerged12 = inner1.merge(inner2)
  //   val innerMerged21 = inner2.merge(inner1)

  //   assertTypedEquals(("d" ->> -1) *: ("e" ->> innerMerged12) *: ("x" ->> "foo") *: EmptyTuple)(outer1.deepMerge(outer2))
  //   assertTypedEquals(("x" ->> "boo") *: ("d" ->> 10) *: ("e" ->> innerMerged21) *: EmptyTuple)(outer2.deepMerge(outer1))

  //   //complete intersection
  //   val inner11 = ("d" ->> "D2") *: ("e" ->> true) *: EmptyTuple
  //   val outer11 = ("d" ->> 11) *: ("e" ->> inner11) *: ("x" ->> "bar") *: EmptyTuple
  //   assertTypedEquals(outer11)(outer1.deepMerge(outer11))
  //   assertTypedEquals(outer1)(outer11.deepMerge(outer1))

  //   //retain type of subrecord if it appears as first parameter
  //   val inner12 = ("e" ->> true) *: ("d" ->> "D12") *: ("x" ->> 5) *: EmptyTuple
  //   test.sameTyped(inner12)(inner12.deepMerge(inner1))
  // }

  test("Extract") {
    val inner1 = ("d" ->> 3) *: ("m" ->> 2D) *: ("x" ->> "X") *: EmptyTuple
    val outer1 = ("x" ->> "foo") *: ("d" ->> -1) *: ("e" ->> inner1) *: EmptyTuple

    type i = ("x" ->> String) *: ("d" ->> Int) *: EmptyTuple
    type i1 = ("x" ->> Any) *: ("d" ->> Any) *: EmptyTuple
    val extRes = ("e" ->> (("x" ->> "X") *: ("d" ->> 3) *: EmptyTuple) *: ("d" ->> -1) *: EmptyTuple)
    assertTypedEquals(extRes)(outer1.extract[("e" ->> i) *: ("d" ->> Int) *: EmptyTuple])
    //covariance
    type e1 = ("e" ->> i1) *: ("d" ->> Any) *: EmptyTuple
    assertEquals(extRes.asInstanceOf[e1], outer1.extract[e1])

    @annotation.unused type ill1 = ("d" ->> Int) *: ("z" ->> Int) *: EmptyTuple
    @annotation.unused type ill2 = ("x" ->> i) *: EmptyTuple
    @annotation.unused type illInner = ("m" ->> String) *: ("d" ->> Int) *: EmptyTuple
    @annotation.unused type ill3 = ("e" ->> illInner) *: ("d" ->> Int) *: EmptyTuple

    illTyped("outer1.extract[ill1]")
    illTyped("outer1.deepExtract[ill2]")
    illTyped("outer1.deepExtract[ill3]")
  }

  test("MergeWith") {
    object mergeField extends Poly2 {
      given xor: Case.Aux[Boolean, Boolean, Boolean] = at[Boolean, Boolean] { _ ^ _ }
      given toDouble: Case.Aux[Int, String, Double] = at[Int, String] { _.toDouble + _.toDouble }
    }

    {
      val r1 = ("c" ->> true) *: EmptyTuple
      val r2 = ("c" ->> false) *: EmptyTuple
      val rExp = ("c" ->> true) *: EmptyTuple

      val rm = r1.mergeWith(r2)(mergeField)
      typed[("c" ->> Boolean) *: EmptyTuple](rm)
      assertEquals(rExp, rm)
    }

    {
      val r1 = ("a" ->> 23) *: ("b" ->> "foo") *: ("c" ->> true) *: EmptyTuple
      val r2 = ("c" ->> false) *: ("a" ->> "13") *: EmptyTuple
      val rExp = ("a" ->> 36.0) *: ("b" ->> "foo") *: ("c" ->> true) *: EmptyTuple

      val rm = r1.mergeWith(r2)(mergeField)
      typed[("a" ->> Double) *: ("b" ->> String) *: ("c" ->> Boolean) *: EmptyTuple](rm)
      assertEquals(rExp, rm)
    }
  }

  test("Concatenate") {
    val r1 =
      (intField1    ->>    23) *:
      (stringField1 ->> "foo") *:
      (boolField1   ->>  true) *:
      (doubleField1 ->>   2.0) *:
      EmptyTuple

    val r2 =
      (intField2    ->>    13) *:
      (stringField2 ->> "bar") *:
      r1

    val v1 = r2.get(intField2)
    typed[Int](v1)
    assertEquals(13, v1)

    val v2 = r2.get(stringField2)
    typed[String](v2)
    assertEquals("bar", v2)
  }

  test("ConcatenateLiteral") {
    val r1 =
      ("intField1"    ->>    23) *:
      ("stringField1" ->> "foo") *:
      ("boolField1"   ->>  true) *:
      ("doubleField1" ->>   2.0) *:
      EmptyTuple

    val r2 =
      ("intField2"    ->>    13) *:
      ("stringField2" ->> "bar") *:
      r1

    val v1 = r2.get("intField2")
    typed[Int](v1)
    assertEquals(13, v1)

    val v2 = r2.get("stringField2")
    typed[String](v2)
    assertEquals("bar", v2)
  }

  test("Append") {
    val r1 =
      (intField1    ->>    23) *:
      (stringField1 ->> "foo") *:
      EmptyTuple

    val r2 = r1 + (boolField1 ->> true)
    typed[intField1.F *: stringField1.F *: boolField1.F *: EmptyTuple](r2)
    assertEquals((intField1 ->> 23) *: (stringField1 ->> "foo") *: (boolField1 ->> true) *: EmptyTuple, r2)

    val r3 = r2 + (doubleField1 ->> 2.0)
    typed[intField1.F *: stringField1.F *: boolField1.F *: doubleField1.F *: EmptyTuple](r3)
    assertEquals((intField1 ->> 23) *: (stringField1 ->> "foo") *: (boolField1 ->> true) *: (doubleField1 ->> 2.0) *: EmptyTuple, r3)
  }

  test("AppendLiteral") {
    val r1 =
      ("intField1"    ->>    23) *:
      ("stringField1" ->> "foo") *:
      EmptyTuple

    val r2 = r1 + ("boolField1" ->> true)
    typed[("intField1" ->> Int) *: ("stringField1" ->> String) *: ("boolField1" ->> Boolean) *: EmptyTuple](r2)
    assertEquals(("intField1" ->> 23) *: ("stringField1" ->> "foo") *: ("boolField1" ->> true) *: EmptyTuple, r2)

    val r3 = r2 + ("doubleField1" ->> 2.0)
    typed[("intField1" ->> Int) *: ("stringField1" ->> String) *: ("boolField1" ->> Boolean) *: ("doubleField1" ->> Double) *: EmptyTuple](r3)
    assertEquals(("intField1" ->> 23) *: ("stringField1" ->> "foo") *: ("boolField1" ->> true) *: ("doubleField1" ->> 2.0) *: EmptyTuple, r3)
  }

  test("Remove") {
    val r1 =
      (intField1    ->>    23) *:
      (stringField1 ->> "foo") *:
      (boolField1   ->>  true) *:
      (doubleField1 ->>   2.0) *:
      EmptyTuple

    val rm1 = r1.remove(intField1)
    typed[(Int, stringField1.F *: boolField1.F *: doubleField1.F *: EmptyTuple)](rm1)
    assertEquals(23, rm1._1)
    assertEquals((stringField1 ->> "foo") *: (boolField1 ->> true) *: (doubleField1 ->> 2.0) *: EmptyTuple, rm1._2)

    val rm2 = r1.remove(stringField1)
    typed[(String, intField1.F *: boolField1.F *: doubleField1.F *: EmptyTuple)](rm2)
    assertEquals("foo", rm2._1)
    assertEquals((intField1 ->> 23) *: (boolField1 ->> true) *: (doubleField1 ->> 2.0) *: EmptyTuple, rm2._2)

    val rm3 = r1.remove(boolField1)
    typed[(Boolean, intField1.F *: stringField1.F *: doubleField1.F *: EmptyTuple)](rm3)
    assertEquals(true, rm3._1)
    assertEquals((intField1 ->> 23) *: (stringField1 ->> "foo") *: (doubleField1 ->> 2.0) *: EmptyTuple, rm3._2)

    val rm4 = r1.remove(doubleField1)
    typed[(Double, intField1.F *: stringField1.F *: boolField1.F *: EmptyTuple)](rm4)
    assertEqualsDouble(2.0, rm4._1, Double.MinPositiveValue)
    assertEquals((intField1 ->> 23) *: (stringField1 ->> "foo") *: (boolField1 ->> true) *: EmptyTuple, rm4._2)

    val r2 = r1 - intField1
    typed[stringField1.F *: boolField1.F *: doubleField1.F *: EmptyTuple](r2)
    assertEquals((stringField1 ->> "foo") *: (boolField1 ->> true) *: (doubleField1 ->> 2.0) *: EmptyTuple, r2)

    val r3 = r1 - stringField1
    typed[intField1.F *: boolField1.F *: doubleField1.F *: EmptyTuple](r3)
    assertEquals((intField1 ->> 23) *: (boolField1 ->> true) *: (doubleField1 ->> 2.0) *: EmptyTuple, r3)

    val r4 = r1 - boolField1
    typed[intField1.F *: stringField1.F *: doubleField1.F *: EmptyTuple](r4)
    assertEquals((intField1 ->> 23) *: (stringField1 ->> "foo") *: (doubleField1 ->> 2.0) *: EmptyTuple, r4)

    val r5 = r1 - doubleField1
    typed[intField1.F *: stringField1.F *: boolField1.F *: EmptyTuple](r5)
    assertEquals((intField1 ->> 23) *: (stringField1 ->> "foo") *: (boolField1 ->> true) *: EmptyTuple, r5)
  }

  test("RemoveLiteral") {
    val r1 =
      ("intField1"    ->>    23) *:
      ("stringField1" ->> "foo") *:
      ("boolField1"   ->>  true) *:
      ("doubleField1" ->>   2.0) *:
      EmptyTuple

    val rm1 = r1.remove("intField1")
    typed[(Int, ("stringField1" ->> String) *: ("boolField1" ->> Boolean) *: ("doubleField1" ->> Double) *: EmptyTuple)](rm1)
    assertEquals(23, rm1._1)
    assertEquals(("stringField1" ->> "foo") *: ("boolField1" ->> true) *: ("doubleField1" ->> 2.0) *: EmptyTuple, rm1._2)

    val rm2 = r1.remove("stringField1")
    typed[(String, ("intField1" ->> Int) *: ("boolField1" ->> Boolean) *: ("doubleField1" ->> Double) *: EmptyTuple)](rm2)
    assertEquals("foo", rm2._1)
    assertEquals(("intField1" ->> 23) *: ("boolField1" ->> true) *: ("doubleField1" ->> 2.0) *: EmptyTuple, rm2._2)

    val rm3 = r1.remove("boolField1")
    typed[(Boolean, ("intField1" ->> Int) *: ("stringField1" ->> String) *: ("doubleField1" ->> Double) *: EmptyTuple)](rm3)
    assertEquals(true, rm3._1)
    assertEquals(("intField1" ->> 23) *: ("stringField1" ->> "foo") *: ("doubleField1" ->> 2.0) *: EmptyTuple, rm3._2)

    val rm4 = r1.remove("doubleField1")
    typed[(Double, ("intField1" ->> Int) *: ("stringField1" ->> String) *: ("boolField1" ->> Boolean) *: EmptyTuple)](rm4)
    assertEqualsDouble(2.0, rm4._1, Double.MinPositiveValue)
    assertEquals(("intField1" ->> 23) *: ("stringField1" ->> "foo") *: ("boolField1" ->> true) *: EmptyTuple, rm4._2)

    val r2 = r1 - "intField1"
    typed[("stringField1" ->> String) *: ("boolField1" ->> Boolean) *: ("doubleField1" ->> Double) *: EmptyTuple](r2)
    assertEquals(("stringField1" ->> "foo") *: ("boolField1" ->> true) *: ("doubleField1" ->> 2.0) *: EmptyTuple, r2)

    val r3 = r1 - "stringField1"
    typed[("intField1" ->> Int) *: ("boolField1" ->> Boolean) *: ("doubleField1" ->> Double) *: EmptyTuple](r3)
    assertEquals(("intField1" ->> 23) *: ("boolField1" ->> true) *: ("doubleField1" ->> 2.0) *: EmptyTuple, r3)

    val r4 = r1 - "boolField1"
    typed[("intField1" ->> Int) *: ("stringField1" ->> String) *: ("doubleField1" ->> Double) *: EmptyTuple](r4)
    assertEquals(("intField1" ->> 23) *: ("stringField1" ->> "foo") *: ("doubleField1" ->> 2.0) *: EmptyTuple, r4)

    val r5 = r1 - "doubleField1"
    typed[("intField1" ->> Int) *: ("stringField1" ->> String) *: ("boolField1" ->> Boolean) *: EmptyTuple](r5)
    assertEquals(("intField1" ->> 23) *: ("stringField1" ->> "foo") *: ("boolField1" ->> true) *: EmptyTuple, r5)
  }

  test("Replace") {
    type R = ("a" ->> Int) *: ("b" ->> String) *: EmptyTuple
    val a = ("a" ->> 1) *: ("b" ->> "2") *: EmptyTuple
    val r = a.replace("a", 2)

    typed[R](r)
    assertEquals(("a" ->> 2) *: ("b" ->> "2") *: EmptyTuple, r)

    illTyped(""" a.replace("a", ()) """)
  }

  test("LacksKey") {
    def without[R <: Tuple, O <: Tuple](k: String)(r: R)(f: R => O)(implicit ev: LacksKey[R, k.type]): O = f(r)

    type R1 = ("a" ->> Int) *: ("b" ->> String) *: ("c" ->> Boolean) *: EmptyTuple
    type R2 = ("c" ->> Boolean) *: ("a" ->> Int) *: ("b" ->> String) *: EmptyTuple

    val a = ("a" ->> 1) *: ("b" ->> "2") *: EmptyTuple

    val r1 = without("c")(a)(_ :+ ("c" ->> true))
    typed[R1](r1)
    assertEquals(("a" ->> 1) *: ("b" ->> "2") *: ("c" ->> true) *: EmptyTuple, r1)

    val r2 = without("c")(a)(("c" ->> true) +: _)
    typed[R2](r2)
    assertEquals(("c" ->> true) *: ("a" ->> 1) *: ("b" ->> "2") *: EmptyTuple, r2)

    illTyped(""" without("a")(a)(identity) """)
  }

  test("RemoveAll") {
    type R = ("i" ->> Int) *: ("s" ->> String) *: ("c" ->> Char) *: ("j" ->> Int) *: EmptyTuple
    type L = ("c" ->> Char) *: ("j" ->> Int) *: EmptyTuple

    type A1 = ("i" ->> Int) *: ("s" ->> String) *: EmptyTuple
    type A2 = Int *: String *: EmptyTuple

    val r = ("i" ->> 10) *: ("s" ->> "foo") *: ("c" ->> 'x') *: ("j" ->> 42) *: EmptyTuple

    val removeAll1 = typify.record.RemoveAll[R, A1]
    val removeAll2 = typify.record.RemoveAll[R, A2]

    val (removed1, remaining1) = removeAll1(r)
    val (removed2, remaining2) = removeAll2(r)

    val r1 = removeAll1.reinsert((removed1, remaining1))
    val r2 = removeAll2.reinsert((removed2, remaining2))

    typed[A1](removed1)
    assertEquals(("i" ->> 10) *: ("s" ->> "foo") *: EmptyTuple, removed1)

    typed[A2](removed2)
    assertEquals(10 *: "foo" *: EmptyTuple, removed2)

    typed[L](remaining1)
    assertEquals(("c" ->> 'x') *: ("j" ->> 42) *: EmptyTuple, remaining1)

    typed[L](remaining2)
    assertEquals(("c" ->> 'x') *: ("j" ->> 42) *: EmptyTuple, remaining2)

    typed[R](r1)
    assertEquals(r, r1)

    typed[R](r2)
    assertEquals(r, r2)
  }

  test("MappingOverRecordFields") {
    object toUpper extends Poly1 {
      given stringToUpper[F]: Case.Aux[F ->> String, F ->> String] = at[F ->> String] {
        f => field[F](f.toUpperCase)
      }

      given otherTypes[X]: Case.Aux[X, X] = at[X](identity)
    }

    val r = ("foo" ->> "joe") *: ("bar" ->> true) *: ("baz" ->> 2.0) *: EmptyTuple
    val r2 = r mapPoly toUpper

    val v1 = r2("foo")
    typed[String](v1)
    assertEquals("JOE", v1)

    val v2 = r2("bar")
    typed[Boolean](v2)
    assertEquals(true, v2)

    val v3 = r2("baz")
    typed[Double](v3)
    assertEqualsDouble(2.0, v3, Double.MinPositiveValue)
  }

  test("UpdateFieldByFunction") {
    val r = ("foo" ->> 23) *: ("bar" ->> true) *: ("baz" ->> 2.0) *: EmptyTuple
    val _ = r.updateWith("foo")((i: Int) => i.toString)
    val _ = r.updateWith("foo")(i => i.toString)
    val r2c = r.updateWith("foo")(_.toString)

    val v21 = r2c.get("foo")
    typed[String](v21)
    assertEquals("23", v21)

    val v22 = r2c("bar")
    typed[Boolean](v22)
    assertEquals(true, v22)

    val v23 = r2c("baz")
    typed[Double](v23)
    assertEqualsDouble(2.0, v23, Double.MinPositiveValue)

    val _ = r.updateWith("foo")((i: Int) => i+1)
    val _ = r.updateWith("foo")(i => i+1)
    val r3c = r.updateWith("foo")(_ + 1)

    val v31 = r3c.get("foo")
    typed[Int](v31)
    assertEquals(24, v31)

    val v32 = r3c("bar")
    typed[Boolean](v32)
    assertEquals(true, v32)

    val v33 = r3c("baz")
    typed[Double](v33)
    assertEqualsDouble(2.0, v33, Double.MinPositiveValue)
  }

  test("Widening") {
    val ps = List(
      ("name"  ->> "Mel")  *:
      ("age"   ->> 90L)    *:
      ("teeth" ->> 2)      *: EmptyTuple,

      ("name"  ->> "Jude") *:
      ("age"   ->> 99L)    *:
      ("teeth" ->> 3)      *: EmptyTuple,

      ("name"  ->> "Bif")  *:
      ("age"   ->> 1L)     *:
      ("teeth" ->> 1)      *: EmptyTuple
    )

    ps.sortBy(_("age"))
    ps.sortBy(_("teeth"))
  }

  test("RenameField") {
    val r = ("foo" ->> 23) *: ("bar" ->> true) *: EmptyTuple
    val r1 = r.renameField("foo", "foobar")

    val v1 = r1.get("foobar")
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r1.get("bar")
    typed[Boolean](v2)
    assertEquals(true, v2)
  }

  test("FieldPoly") {
    object f extends FieldPoly {
      given atFoo: Case.Aux["foo" ->> Int, "foo" ->> Int] = atField[Int]("foo")(_ + 1)
    }

    val r = "foo" ->> 23
    val r1 = f(r)

    typed["foo" ->> Int](r1)
    assertEquals(24, r1: Int)
  }

  test("SelectDynamic") {
    val r = ("foo" ->> 23) *: ("bar" ->> true) *: EmptyTuple
    val d = r.record

    val v1 = d.foo
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = d.bar
    typed[Boolean](v2)
    assertEquals(true, v2)

    illTyped("d.baz")
  }

  test("RecordTypeSelector") {
    typed[EmptyTuple](EmptyTuple)

    typed[("i" ->> Int) *: EmptyTuple](("i" ->> 23) *: EmptyTuple)

    typed[("i" ->> Int) *: ("s" ->> String) *: EmptyTuple](("i" ->> 23) *: ("s" ->> "foo") *: EmptyTuple)

    typed[("i" ->> Int) *: ("s" ->> String) *: ("b" ->> Boolean) *: EmptyTuple](("i" ->> 23) *: ("s" ->> "foo") *: ("b" ->> true) *: EmptyTuple)
  }

  test("NamedArgsInject") {
    val r = ("i" ->> 23) *: ("s" ->> "foo") *: ("b" ->> true) *: EmptyTuple

    val v1 = r.get("i")
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r.get("s")
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r.get("b")
    typed[Boolean](v3)
    assertEquals(true, v3)

    illTyped("""r.get("foo")""")
  }

  test("Fields") {
    {
      val f = EmptyTuple.fields
      assertTypedEquals(EmptyTuple, f)
    }

    {
      val f = (EmptyTuple: EmptyTuple).fields
      assertTypedEquals(EmptyTuple: EmptyTuple, f)
    }

    val r = ("i" ->> 23) *: ("s" ->> "foo") *: ("b" ->> true) *: EmptyTuple

    {
      val f = r.fields
      assertTypedEquals(("i" -> 23) *: ("s" -> "foo") *: ("b" -> true) *: EmptyTuple, f)
    }

    val rs = ("first" ->> Some(2)) *: ("second" ->> Some(true)) *: ("third" ->> Option.empty[String]) *: EmptyTuple

    {
      val f = rs.fields
      assertTypedEquals(("first" -> Some(2)) *: ("second" -> Some(true)) *: ("third" -> Option.empty[String]) *: EmptyTuple, f)
    }
  }

  test("UnzipFields") {
    {
      val uf = UnzipFields[EmptyTuple]
      assertTypedEquals(EmptyTuple, uf.keys)
      assertTypedEquals(EmptyTuple, uf.values(EmptyTuple))
    }

    {
      val uf = UnzipFields[EmptyTuple]
      assertTypedEquals(EmptyTuple: EmptyTuple, uf.keys)
      assertTypedEquals(EmptyTuple: EmptyTuple, uf.values(EmptyTuple: EmptyTuple))
    }

    type R = ("i" ->> Int) *: ("s" ->> String) *: ("b" ->> Boolean) *: EmptyTuple
    val r: R = ("i" ->> 23) *: ("s" ->> "foo") *: ("b" ->> true) *: EmptyTuple

    {
      val uf = UnzipFields[R]
      assertTypedEquals("i" *: "s" *: "b" *: EmptyTuple, uf.keys)
      assertTypedEquals(23 *: "foo" *: true *: EmptyTuple, uf.values(r))
    }

    type RS = ("first" ->> Option[Int]) *: ("second" ->> Option[Boolean]) *: ("third" ->> Option[String]) *: EmptyTuple
    val rs: RS = ("first" ->> Some(2)) *: ("second" ->> Some(true)) *: ("third" ->> Option.empty[String]) *: EmptyTuple

    {
      val uf = UnzipFields[RS]
      assertTypedEquals("first" *: "second" *: "third" *: EmptyTuple, uf.keys)
      assertTypedEquals(Some(2) *: Some(true) *: Option.empty[String] *: EmptyTuple, uf.values(rs))
    }
  }

  test("ToMap") {
    {
      val m = EmptyTuple.toMap
      assertTypedEquals(Map.empty[Any, Nothing], m)
    }

    {
      val m = EmptyTuple.toMap[String, Nothing]
      assertTypedEquals(Map.empty[String, Nothing], m)
    }

    {
      val m = EmptyTuple.toMap[String, Int]
      assertTypedEquals(Map.empty[String, Int], m)
    }

    val r = ("i" ->> 23) *: ("s" ->> "foo") *: ("b" ->> true) *: EmptyTuple

    {
      val m = r.toMap
      assertTypedEquals(Map[String, Any]("i" -> 23, "s" -> "foo", "b" -> true), m)
    }

    {
      val m = r.toMap[String, Any]
      assertTypedEquals(Map[String, Any]("i" -> 23, "s" -> "foo", "b" -> true), m)
    }

    val rs = ("first" ->> Some(2)) *: ("second" ->> Some(true)) *: ("third" ->> Option.empty[String]) *: EmptyTuple

    {
      val m = rs.toMap
      assertTypedEquals(Map[String, Option[Any]]("first" -> Some(2), "second" -> Some(true), "third" -> Option.empty[String]), m)
    }

    {
      val m = rs.toMap[String, Option[Any]]
      assertTypedEquals(Map[String, Option[Any]]("first" -> Some(2), "second" -> Some(true), "third" -> Option.empty[String]), m)
    }
  }

  test("MapValues") {
    object f extends Poly1 {
      given int: Case.Aux[Int, Boolean] = at[Int](i => i > 0)
      given string: Case.Aux[String, String] = at[String](s => s"s: $s")
      given boolean: Case.Aux[Boolean, String] = at[Boolean](v => if (v) "Yup" else "Nope")
    }

    {
      val r = EmptyTuple
      val res = r.mapValues(f)
      assertTypedEquals[EmptyTuple](EmptyTuple, res)
    }

    {
      val r = ("i" ->> 23) *: ("s" ->> "foo") *: ("b" ->> true) *: EmptyTuple
      val res = r.mapValues(f)
      assertTypedEquals[("i" ->> Boolean) *: ("s" ->> String) *: ("b" ->> String) *: EmptyTuple](
        ("i" ->> true) *: ("s" ->> "s: foo") *: ("b" ->> "Yup") *: EmptyTuple,
        res,
      )
    }

    {
      object toUpper extends Poly1 {
        given stringToUpper: Case.Aux[String, String] = at[String](_.toUpperCase)
        given otherTypes[X]: Case.Aux[X, X] = at[X](identity)
      }

      val r = ("foo" ->> "joe") *: ("bar" ->> true) *: ("baz" ->> 2.0) *: EmptyTuple
      val r2 = r mapValues toUpper

      val v1 = r2("foo")
      typed[String](v1)
      assertEquals("JOE", v1)

      val v2 = r2("bar")
      typed[Boolean](v2)
      assertEquals(true, v2)

      val v3 = r2("baz")
      typed[Double](v3)
      assertEqualsDouble(2.0, v3, Double.MinPositiveValue)
    }
  }

  test("SwapRecord") {
    type TestRecord = ("x" ->> Int) *: ("y" ->> String) *: ("z" ->> Boolean) *: EmptyTuple

    val fields: ((Int ->> "x") *: (String ->> "y") *: (Boolean ->> "z") *: EmptyTuple) = SwapRecord[TestRecord].apply()

    assertEquals(fields.values.toListLub, List("x", "y", "z"))
  }

  test("AlignByKeys") {
    type TestRecord = ("a" ->> String) *: ("b" ->> Int) *: ("c" ->> Double) *: EmptyTuple

    type Keys1 = "a" *: "b" *: "c" *: EmptyTuple
    type Keys2 = "b" *: "c" *: "a" *: EmptyTuple
    type Keys3 = "b" *: "a" *: "c" *: EmptyTuple
    type Keys4 = "c" *: "a" *: "b" *: EmptyTuple

    val v = ("a" ->> "foo") *: ("b" ->> 42) *: ("c" ->> 33.3) *: EmptyTuple

    assertTypedEquals[TestRecord](v, AlignByKeys[TestRecord, Keys1].apply(v))
    assertTypedEquals[("b" ->> Int) *: ("c" ->> Double) *: ("a" ->> String) *: EmptyTuple](
      ("b" ->> 42) *: ("c" ->> 33.3) *: ("a" ->> "foo") *: EmptyTuple,
      AlignByKeys[TestRecord, Keys2].apply(v),
    )

    assertTypedEquals[("b" ->> Int) *: ("a" ->> String) *: ("c" ->> Double) *: EmptyTuple](
      ("b" ->> 42) *: ("a" ->> "foo") *: ("c" ->> 33.3) *: EmptyTuple,
      v.alignByKeys[Keys3],
    )

    assertTypedEquals[("c" ->> Double) *: ("a" ->> String) *: ("b" ->> Int) *: EmptyTuple](
      ("c" ->> 33.3) *: ("a" ->> "foo") *: ("b" ->> 42) *: EmptyTuple,
      v.alignByKeys[Keys4],
    )
  }

  test("SelectorWithTaggedType") {
    val tagged = translucentTag[Int]("42")
    val head1 = "k" ->> tagged
    val head2 = field["k"](tagged)
    val rec1 = head1 *: EmptyTuple
    val rec2 = head2 *: EmptyTuple

    assertTypedEquals[String @@ Int](rec1("k"), rec2("k"))
  }

  test("SelectorWithTaggedType2") {
    trait TestTag
    case class FooT(bar: String @@ TestTag)
    val lgt = LabelledGeneric[FooT]
    val fooT = FooT(translucentTag[TestTag]("test"))

    assertEquals(translucentTag[TestTag]("test"), lgt.to(fooT).get("bar"))
  }

  test("SelectorForSwappedRecord") {
    val gen = LabelledGeneric[Bar]
    val swap = SwapRecord[gen.Repr]
    val select = typify.record.Selector[swap.Out, Int]
    val swapped = swap()

    assertTypedEquals["a"](swapped.head, select(swapped))
 }

  test("FieldTypeOfValueClass") {
    val x = RecordTests.aValueClassField ->> RecordTests.AValueClass(1L)
    assertEquals(x.l, Array(x).apply(0).l)
  }
}

object RecordTests {
  case class AValueClass(l: Long) extends AnyVal
  object aValueClassField extends FieldOf[AValueClass]
}
