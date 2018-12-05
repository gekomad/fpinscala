import org.scalatest.FunSuite

object IOMonad {

  sealed trait TailRec[A] { // IO[A]
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)

    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends TailRec[A]

  case class Suspend[A](resume: () => A) extends TailRec[A]

  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)

    def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f

    def suspend[A](a: => TailRec[A]): TailRec[A] = Suspend(() => ()).flatMap { _ => a }

    def apply[A](a: => A): TailRec[A] = unit(a)

    def forever[A, B](a: TailRec[A]): TailRec[B] = {
      lazy val t: TailRec[B] = forever(a)
      a flatMap (_ => t)
    }
  }

  def printLine(s: String): TailRec[Unit] = Suspend(() => println(s))

  @annotation.tailrec def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (g(_) flatMap f))
    }
  }
}

import IOMonad._

object Main extends App {
  def ReadLine: TailRec[String] = TailRec {
    scala.io.StdIn.readLine()
  }

  def printLine(s: String): TailRec[Unit] = Suspend(() => println(s))

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: TailRec[Unit] = for {
    _ <- printLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- printLine(fahrenheitToCelsius(d).toString)
  } yield ()


  run(converter)
  //  run(IO.forever(printLine("Still going...")))

}

class Ch13_External_effects_and_IO extends FunSuite {

  test("Trampolining") {

    val f: Int => TailRec[Int] = (x: Int) => Return(x)

    val g = List.fill(100000)(f).foldLeft(f) {
      (a, b) => x => Suspend(() => ()).flatMap { _ => a(x + 1).flatMap(b) }
    }

    assert(IOMonad.run(g(42)) == 100042)
  }
}
