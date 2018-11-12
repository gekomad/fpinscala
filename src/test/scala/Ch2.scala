import org.scalatest.FunSuite

import scala.annotation.tailrec

class Ch2 extends FunSuite {

  test("EXERCISE 2.1 fibonacci") {

    def fib(n: Int): Int = if (n == 0) 0 else if (n < 3) 1 else
      fib(n - 1) + fib(n - 2)

    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(4) == 3)
    assert(fib(5) == 5)
    assert(fib(6) == 8)
    assert(fib(7) == 13)
    assert(fib(8) == 21)
    assert(fib(12) == 144)
    assert(fib(15) == 610)

  }

  test("EXERCISE 2.2 isSorted") {

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

      @tailrec
      def go(n: Int): Boolean = if (n >= as.length - 1)
        true
      else if (!ordered(as(n), as(n + 1)))
        false
      else
        go(n + 1)

      go(0)
    }


    assert(isSorted(Array(), (a: Int, b: Int) => a <= b))
    assert(isSorted(Array(1), (a: Int, b: Int) => a <= b))
    assert(isSorted(Array(1, 2, 3), (a: Int, b: Int) => a <= b))
    assert(isSorted(Array(1, 2, 2, 3), (a: Int, b: Int) => a <= b))
    assert(!isSorted(Array(1, 3, 2), (a: Int, b: Int) => a <= b))
  }

  test("EXERCISE 2.3 curry") {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

    assert(curry((a: Int, b: Double) => a.toString + " " + b.toString)(1)(1.2) == "1 1.2")
  }

  test("EXERCISE 2.4 uncurry") {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

    assert(uncurry((a: Int) => (b: Double) => a.toString + " " + b.toString)(1, 1.2) == "1 1.2")
  }


  test("EXERCISE 2.5 compose") {

    def compose1[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

    val res = s"(2)"
    assert(compose1((a: Int) => s"($a)", (a: Int) => a + 1)(1) == res)

    def compose2[A, B, C](f: B => C, g: A => B): A => C = a => (g andThen f) (a)

    assert(compose2((a: Int) => s"($a)", (a: Int) => a + 1)(1) == res)
  }
}
