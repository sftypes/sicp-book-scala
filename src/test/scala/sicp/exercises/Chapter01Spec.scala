package sicp.exercises

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

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

  behavior of "counting change"

  def counting_change_every_way(amount: Int, coins: List[Int]): Seq[List[Int]] = coins match {
    case head :: rest ⇒
      if (amount < 0) Seq() else if (amount == 0) Seq(Nil) else {
        counting_change_every_way(amount, rest) ++ counting_change_every_way(amount - head, coins).map(head :: _)
      }
    case Nil ⇒ Seq()
  }

  it should "count change correctly for some examples" in {
    val us_coins = List(1, 5, 10, 25, 50, 100, 200)
    counting_change_every_way(0, List(1, 5)) shouldEqual Seq(Nil)

    // Payout is impossible.
    counting_change_every_way(5, List(2, 7)) shouldEqual Seq()

    counting_change_every_way(12, List(2, 7)) shouldEqual Seq(List(2, 2, 2, 2, 2, 2))

    counting_change_every_way(11, List(2, 7)) shouldEqual Seq(
      List(2, 2, 7)
    )

    counting_change_every_way(10, us_coins) shouldEqual Seq(
      List(10),
      List(5, 5),
      List(1, 1, 1, 1, 1, 5),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    )

    counting_change_every_way(20, us_coins) shouldEqual Seq(
      List(10, 10),
      List(5, 5, 10),
      List(5, 5, 5, 5),
      List(1, 1, 1, 1, 1, 5, 10),
      List(1, 1, 1, 1, 1, 5, 5, 5),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 10),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
  }

  behavior of "exercise 1.11"

  def solution_1_11_rec(n: Int): Int = {
    if (n < 3)
      n
    else
      solution_1_11_rec(n - 1) + 2 * solution_1_11_rec(n - 2) + 3 * solution_1_11_rec(n - 3)
  }

  def solution_1_11_iter(n: Int): Int = {

    // Auxiliary function: arguments are at the end a = f(n-2), b = f(n-1), c = f(n), count = n
    @tailrec
    def g(a: Int, b: Int, c: Int, count: Int): Int = {
      if (count < 3)
        c
      else g(b, c, c + 2 * b + 3 * a, count - 1)
    }

    g(0, 1, 2, n)
  }

  it should "compute using recursion" in {
    solution_1_11_rec(2) shouldEqual 2
    solution_1_11_rec(3) shouldEqual 4
    solution_1_11_rec(10) shouldEqual 1892
  }

  it should "compute using iteration" in {
    solution_1_11_iter(2) shouldEqual 2
    solution_1_11_iter(3) shouldEqual 4
    solution_1_11_iter(10) shouldEqual 1892
  }

  behavior of "exercise 1.12"

  def solution_1_12_rec(m: Int, n: Int): Int = {
    if (n > m || n < 0 || m < 0) 0 else if (n == m || n == 0 || m == 0) 1 else solution_1_12_rec(m - 1, n) + solution_1_12_rec(m - 1, n - 1)
  }

  it should "compute some examples correctly" in {
    solution_1_12_rec(0, 0) shouldEqual 1
    solution_1_12_rec(1, 1) shouldEqual 1
    solution_1_12_rec(2, 1) shouldEqual 2
    solution_1_12_rec(4, 0) shouldEqual 1
    solution_1_12_rec(4, 2) shouldEqual 6
  }

  behavior of "exercise 1.13"

  def fib(n: Int): Int = if (n <= 2) 1 else fib(n - 1) + fib(n - 2)

  def phi_n(n: Int): Long = math.round(math.pow((math.sqrt(5) + 1.0) / 2.0, n) / math.sqrt(5))

  it should "check the statement for some n" in {
    ((1 to 10) ++ Seq(20, 30, 40)).foreach { n ⇒
      phi_n(n) shouldEqual fib(n)
    }
  }

  behavior of "smallest-divisor and friends"

  def unfold[T](z: T)(update: T ⇒ T): Iterator[T] = new Iterator[T] {
    private var current: T = z

    override def hasNext: Boolean = true

    override def next(): T = {
      val last = current
      current = update(current)
      last
    }
  }

  it should "unfold sequence" in {
    unfold(0)(_ + 1).take(5).toSeq shouldEqual (0 to 4)
  }

  def smallest_divisor(n: Int): Int = (2 to n).filter(i ⇒ i * i <= n).find(i ⇒ n % i == 0).getOrElse(n)

  // Exercise 1.21
  it should "find smallest divisors" in {
    println("Smallest divisors are" + Seq(199, 1999, 19999).map(smallest_divisor))
  }

  behavior of "exercise 1.29"

  // Return the coefficient at i-th term in the sequence (1/3, 4/3, 2/3, ..., 4/3, 1/3) where n must be a multiple of 4
  def simpson_coeff(i: Int, n: Int): Double = {
    if (i < 0 || i > n || n % 4 != 0)
      0
    else {
      val c = if (i == 0 || i == n) 1 else if (i % 2 == 0) 2 else 4
      c / 3.0
    }
  }

  def simpson_integral(f: Double ⇒ Double, range: (Double, Double), grid_size: Int): Double = {
    val (left, right) = range
    val n = 4 * grid_size
    (0 to n).map(i ⇒ f(left + (right - left) * i / n) * simpson_coeff(i, n)).sum * (right - left) / n
  }

  it should "compute integral of x^3 exactly" in {
    val result = simpson_integral(x ⇒ x * x * x, (0, 1), 10)
    result shouldEqual 1.0 / 4.0
  }

  it should "compute integral of x^5 approximately" in {
    val result = simpson_integral(x ⇒ x * x * x * x * x, (0, 1), 50)
    result shouldEqual 1.0 / 6.0 +- 1e-9
  }

}
