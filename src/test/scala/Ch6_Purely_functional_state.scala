package ch6

import org.scalatest.FunSuite

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG: SimpleRNG = SimpleRNG(newSeed)
    val n: Int = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object SimpleRNG {
  // EXERCISE 6.1 nonNegativeInt
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) i + Int.MaxValue else i, r)
  }
}


//noinspection NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters,NameBooleanParameters
class Ch6_Purely_functional_state extends FunSuite {

  import SimpleRNG._

  // EXERCISE 6.2 generate Double

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, r)
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    val o: (B, RNG) = (f(a), rng2)
    o
  }

  test("EXERCISE 6.1 nonNegativeInt") {

    assert(nonNegativeInt(SimpleRNG(42))._1 == 16159453)
    assert(nonNegativeInt(SimpleRNG(0))._1 == 0)
    assert(nonNegativeInt(SimpleRNG(Int.MinValue))._1 == 214532095)
    assert(nonNegativeInt(SimpleRNG(Int.MaxValue))._1 == 1932566803)
  }

  test("EXERCISE 6.2 generate Double") {
    assert(double(SimpleRNG(Int.MaxValue))._1 == 0.8999215457122408)
    assert(double(SimpleRNG(Int.MinValue))._1 == 0.09989929157304545)
    assert(double(SimpleRNG(0))._1 == 0.0)
    assert(double(SimpleRNG(42))._1 == 0.007524831689672932)
  }

  test("EXERCISE 6.3 pairs") {

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r) = nonNegativeInt(rng)
      val (d, rn) = nonNegativeInt(r)
      ((i, d), rn)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (d, r) = nonNegativeInt(rng)
      val (i, rn) = nonNegativeInt(r)
      ((i, d), rn)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d, r) = nonNegativeInt(rng)
      val (d1, r1) = nonNegativeInt(r)
      val (d2, r2) = nonNegativeInt(r1)
      ((d, d1, d2), r2)
    }

    assert(intDouble(SimpleRNG(42))._1 == (16159453, 8.6600395E8))
    assert(doubleInt(SimpleRNG(42))._1 == (8.6600395E8, 16159453))
    assert(double3(SimpleRNG(42))._1 == (1.6159453E7, 8.6600395E8, 1.807177745E9))

  }

  test("EXERCISE 6.4 list of random integers") {

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

      def go(count: Int)(rng: RNG): (List[Int], RNG) = {
        if (count == 0) (Nil, rng) else {
          val (i, r) = nonNegativeInt(rng)
          val (a, b) = go(count - 1)(r)
          (i :: a, b)
        }
      }

      go(count)(rng)
    }

    assert(ints(5)(SimpleRNG(42))._1 == List(16159453, 866003950, 1807177745, 131727627, 1770001318))

  }

  test("EXERCISE 6.5 double using map") {

    def double(rng: RNG): (Double, RNG) = {

      def f(rng: RNG): (Double, RNG) = {
        val (i, r) = rng.nextInt
        (i.toDouble, r)
      }

      map(f)(_ / Int.MaxValue)(rng)
    }

    assert(double(SimpleRNG(42))._1 == 0.007524831689672932)
  }

  test("EXERCISE 6.6 map2") {

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng: RNG) => {
      val (a, r) = ra(rng)
      val (b, r2) = rb(r)
      val c: C = f(a, b)
      (c, r2)
    }

    val int1: Rand[Int] = (a: RNG) => a.nextInt
    val int2: Rand[Int] = (a: RNG) => a.nextInt
    // A :Int B: Int C: Int
    assert(map2(int1, int2)((a: Int, b: Int) => a + b)(SimpleRNG(42))._1 == -1265320244)


    val double1: Rand[Double] = (a: RNG) => {
      val (i, r) = a.nextInt
      (i.toDouble / Int.MaxValue, r)
    }

    // A :Int B: Double C: Float
    assert(map2(int1, double1)((a: Int, b: Double) => (a + b).toFloat)(SimpleRNG(42))._1 == 1.6159452E7)

    //
    val int: Rand[Int] = _.nextInt

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = both(int, double)
    val randDoubleInt: Rand[(Double, Int)] = both(double, int)

    assert(randIntDouble(SimpleRNG(42))._1 == (16159453, 0.40326451435837174))
    assert(randDoubleInt(SimpleRNG(42))._1 == (0.007524831689672932, -1281479697))
  }

  test("EXERCISE 6.8 flatMap") {

    def flatMap[A, B](f: RNG => (A, RNG))(g: A => RNG => (B, RNG)): RNG => (B, RNG) = (rng: RNG) => {
      val x: (A, RNG) = f(rng)
      val a: (B, RNG) = g(x._1)(x._2)
      a
    }

    val int1: Rand[Int] = (a: RNG) => a.nextInt

    val int2: Int => Rand[Double] = (x: Int) => (a: RNG) => {
      val (i, r) = a.nextInt
      ((i + x).toDouble, r)
    }

    assert(flatMap(int1)(int2)(SimpleRNG(42))._1 == -1.265320244E9)

  }

  test("EXERCISE 6.11 candy machine") {
    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    final case class Machine(locked: Boolean, candies: Int, coins: Int)

    type State[+M, I] = Machine => (M, I)
    type State2[+M] = Machine => M

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = { machine =>

      //noinspection NameBooleanParameters
      def go(input: Input): State2[Machine] = machine =>
        (input, machine) match {
          case (Coin, Machine(true, candies, coins)) if candies > 0 =>
            println("1 Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.")
            Machine(false, candies, coins + 1)
          case (Turn, Machine(false, candies, coins)) =>
            println("2 Turning the knob on an unlocked machine will cause it to dispense candy and become locked.")
            Machine(true, candies - 1, coins)
          case (Turn, m@Machine(true, _, _)) =>
            println("3 Turning the knob on a locked machine does nothing.")
            m
          case (Coin, m@Machine(false, _, _)) =>
            println("4 Inserting a coin into an unlocked machine does nothing.")
            m
          case (_, m@Machine(_, 0, _)) =>
            println("5 A machine that’s out of candy ignores all inputs.")
            m
        }


      inputs.foldLeft((machine, (machine.coins, machine.candies)))((m, i) => {
        val o = go(i)(m._1)
        (o, (o.coins, o.candies))
      })
    }

    println("test 1a")
    assert(simulateMachine(List(Coin))(Machine(true, 1, 2)) == (Machine(false, 1, 3), (3, 1)))

    println("test 1b")
    assert(simulateMachine(List(Coin))(Machine(true, 0, 2)) == (Machine(true, 0, 2), (2, 0)))

    println("test 2a")
    assert(simulateMachine(List(Turn))(Machine(false, 1, 2)) == (Machine(true, 0, 2), (2, 0)))

    println("test 3a")
    assert(simulateMachine(List(Turn))(Machine(true, 1, 2)) == (Machine(true, 1, 2), (2, 1)))

    println("test 4a")
    assert(simulateMachine(List(Coin))(Machine(false, 1, 2)) == (Machine(false, 1, 2), (2, 1)))

    println("test 5a")
    assert(simulateMachine(List(Turn))(Machine(true, 0, 2)) == (Machine(true, 0, 2), (2, 0)))

    println("test 5b")
    assert(simulateMachine(List(Coin))(Machine(true, 0, 2)) == (Machine(true, 0, 2), (2, 0)))

    println("test a")
    assert(simulateMachine(List(Coin, Turn))(Machine(true, 1, 2)) == (Machine(true, 0, 3), (3, 0)))

    println("test b")
    assert(simulateMachine(List(Turn, Turn, Coin, Turn))(Machine(true, 1, 2)) == (Machine(true, 0, 3), (3, 0)))

    println("test c")
    assert(simulateMachine(List(Coin, Turn))(Machine(true, 0, 2)) == (Machine(true, 0, 2), (2, 0)))

  }

}
