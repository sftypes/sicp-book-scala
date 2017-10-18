package sicp.exercises

import org.scalatest.{FlatSpec, Matchers}

class Chapter01Spec extends FlatSpec with Matchers {

  behavior of "exercise 1.3"

  def solution_1_3(x: Int, y: Int, z: Int): Int = {
    def sum_squares(pair: (Int, Int)): Int = pair._1 * pair._1 + pair._2 * pair._2

    sum_squares {
      if (x < y) {
        if (x < z) (y, z) else (x, y)
      } else {
        if (y < z) (x, z) else (x, y)
      }
    }
  }

  def solution_1_3a(x: Int, y: Int, z: Int): Int = {
    Seq(x, y, z).sorted.tail.map(x ⇒ x * x).sum
  }

  it should "work with nested `if` expressions" in {
    solution_1_3(1, 2, 3) shouldEqual 2 * 2 + 3 * 3
    solution_1_3(1, 2, 2) shouldEqual 2 * 2 + 2 * 2
    solution_1_3(1, 1, 2) shouldEqual 1 * 1 + 2 * 2
    solution_1_3(0, 0, 0) shouldEqual 0
    solution_1_3(1, 2, -3) shouldEqual 2 * 2 + 1 * 1
  }

  it should "work with sequence" in {
    solution_1_3a(1, 2, 3) shouldEqual 2 * 2 + 3 * 3
    solution_1_3a(1, 2, 2) shouldEqual 2 * 2 + 2 * 2
    solution_1_3a(1, 1, 2) shouldEqual 1 * 1 + 2 * 2
    solution_1_3a(0, 0, 0) shouldEqual 0
    solution_1_3a(1, 2, -3) shouldEqual 2 * 2 + 1 * 1
  }

  behavior of "exercise 1.4"

  def a_plus_abs_b(a: Int, b: Int): Int = {
    val func: (Int, Int) ⇒ Int = if (b > 0) (_ + _) else (_ - _)
    func(a, b)
  }

  it should "select function by condition" in {
    a_plus_abs_b(1, 2) shouldEqual 3
    a_plus_abs_b(1, -2) shouldEqual 3
  }

  behavior of "exercise 1.5"

  def p: Int = p

  def test_applicative_order(x: Int, y: Int) = if (x == 0) 0 else y

  def test_normal_order(x: ⇒ Int, y: ⇒ Int) = if (x == 0) 0 else y

  it should "evaluate in applicative order" in {
    the[StackOverflowError] thrownBy {
      val result = test_applicative_order(0, p)
      result shouldEqual 0
    } should have message null
  }

  it should "evaluate in normal order" in {
    val result = test_normal_order(0, p)
    result shouldEqual 0
  }

}
