package ch8
import ch6.{RNG, SimpleRNG}
import org.scalatest.funsuite.AnyFunSuite
import ch7._
import ch5._
import MyStream._

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(SimpleRNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def boolean: Gen[Boolean] = {
    val c: State[RNG, Boolean] = State(_.nextInt match { case (i, r) => (i % 2 == 0, r) })
    Gen(c)
  }

  type TestCases = Int

  case class Prop(run: (TestCases, RNG) => Result) {
    def &&(p: Prop) = Prop {
      (max, n) =>
        run(max, n) match {
          case Passed => p.run(max, n)
          case x => x
        }

    }

    def ||(p: Prop) = Prop {
      (max, n) =>
        run(max, n) match {
          // In case of failure, run the other prop.
          case Falsified(_, _) =>
            p.run(max, n)
          case
            x => x
        }
    }

  }

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = {
    Prop {
      (n, rng) =>
        randomStream(as)(rng).zip(from(0)).take(n).map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception =>
              Falsified(buildMsg(a, e), i)
          }
        }.find(_.isFalsified).getOrElse(Passed)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

import Prop._
import ch6._

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Prop.listOfN(n, this))

}

class Ch8_Property_based_testing extends AnyFunSuite {

  import Prop._

  test("EXERCISE 8.4 choose") {
    val c: Gen[Int] = choose(0, 100)
    assert(c.sample.run(SimpleRNG(42))._1 == 53)
  }

  test("EXERCISE 8.5") {

    {
      assert(unit(42).sample.run(new RNG {
        def nextInt: (Int, RNG) = (???, ???)
      })._1 == 42)
    }

    {
      assert(!boolean.sample.run(SimpleRNG(42))._1)
      assert(boolean.sample.run(SimpleRNG(41))._1)

    }

    {
      val p = listOfN(16, choose(0, 100))
      assert(p.sample.run(SimpleRNG(42))._1 == List(53, 50, 45, 27, 18, 88, 12, 6, 88, 74, 53, 32, 30, 56, 81, 21))
    }

  }

  test("Generate Gen[(Int,Int)]") {
    val c: Gen[Int] = choose(0, 100)
    val p = c.sample.run(SimpleRNG(42))
    val p2 = c.sample.run(p._2)
    assert((p._1, p2._1) == (53, 50))
  }

  test("Gen[Option[A]] from a Gen[A]") {
    val c: Gen[Int] = choose(0, 100)

    val ps = c.sample.map2(State(_.nextInt match { case (i, r) => (i % 2 == 0, r) }))((x, b) => if (b) Some(x) else None)

    assert(ps.run(SimpleRNG(42))._1.isEmpty)
    assert(ps.run(SimpleRNG(10))._1.contains(89))
  }

  test("EXERCISE 8.6 flatMap") {
    val p = unit(42).listOfN(unit(3))
    assert(p.sample.run(SimpleRNG(42))._1 == List(42, 42, 42))

    val p1 = choose(0, 10).listOfN(unit(3))
    assert(p1.sample.run(SimpleRNG(42))._1 == List(3, 0, 5))
  }

  test("EXERCISE 8.7 union") {

    val c1: Gen[Int] = unit(41)
    val c2: Gen[Int] = unit(42)

    assert(union(c1, c2).sample.run(SimpleRNG(2))._1 == 42)
    assert(union(c1, c2).sample.run(SimpleRNG(3))._1 == 41)
  }

  test("EXERCISE 8.9 && ||") {
    val nine = unit(9)

    {

      val combinedProp = (forAll(nine)(_ < 10) ||
        forAll(nine)(_ < 21)) &&
        forAll(nine)(_ >= 0)

      val result = combinedProp.run(100, SimpleRNG(4))
      assert(result == Passed)
    }

    {

      val combinedProp = (forAll(nine)(_ > 10) ||
        forAll(nine)(_ < 8)) &&
        forAll(nine)(_ >= 0)

      val result = combinedProp.run(100, SimpleRNG(4))
      assert(result == Falsified("9", 0))
    }

    {
      val combinedProp = forAll(nine)(_ > 1)

      val result = combinedProp.run(100, SimpleRNG(4))
      assert(result == Passed)
    }

    {

      val combinedProp: Prop = forAll(nine)(_ < 0) || forAll(nine)(_ >= 110)

      val result: Result = combinedProp.run(100, SimpleRNG(4))
      assert(result == Falsified("9", 0))
    }
  }
}
