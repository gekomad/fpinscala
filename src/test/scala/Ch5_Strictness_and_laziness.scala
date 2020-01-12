package ch5

import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.tailrec

object MyStream {

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head: A         = hd
      lazy val tail: Stream[A] = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  import Stream._

  sealed trait Stream[+A] {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

    @tailrec
    final def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
      case _          => z
    }

    //EXERCISE 5.1 Stream to List
    def toList: List[A] = this match {
      case Empty      => List.empty[A]
      case Cons(h, t) => h.apply() :: t.apply().toList
    }

    //EXERCISE 5.2 take
    def take(n: Int): Stream[A] = this match {
      case Empty       => Empty
      case _ if n == 0 => Empty
      case Cons(h, t)  => cons(h.apply(), t.apply().take(n - 1))
    }

    //EXERCISE 5.2 drop
    def drop(n: Int): Stream[A] = this match {
      case Empty       => Empty
      case Cons(x, xs) => if (n == 0) Cons(x, xs) else xs.apply().drop(n - 1)
    }

    //EXERCISE 5.3 takeWhile
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) =>
        cons(h(), t().takeWhile(p))
      case _ =>
        Empty
    }

    //EXERCISE 5.4 forAll
    def forAll(p: A => Boolean): Boolean = this match {
      case Empty                => true
      case Cons(h, t) if p(h()) => t().forAll(p)
      case _                    => false
    }

    def find(p: A => Boolean): Option[A] = this match {
      case Empty      => None
      case Cons(h, t) => if (p(h())) Some(h()) else t().find(p)
    }

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }

    def zip[B](s2: Stream[B]): Stream[(A, B)] =
      zipWith(s2)((_, _))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(f(h), t))

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  import Stream._

  // EXERCISE 5.11 unfold

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some(x) =>
      val (nextValue, nextState) = x
      cons(nextValue, unfold(nextState)(f))
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

}

import MyStream._
import MyStream.Stream._

class Ch5_Strictness_and_laziness extends AnyFunSuite {

  test("EXERCISE 5.1 Stream to List") {
    assert(cons(1, Empty).toList == List(1))
    assert(cons(1, cons(2, Empty)).toList == List(1, 2))
  }

  test("EXERCISE 5.2 take and drop") {
    assert(cons(1, cons(2, Empty)).take(1).toList == List(1))
    assert(cons(1, cons(2, Empty)).drop(1).toList == List(2))
  }

  test("EXERCISE 5.3 takeWhile") {
    assert(cons(1, cons(2, Empty)).takeWhile((a: Int) => a < 2).toList == List(1))
    assert(cons(1, cons(2, Empty)).takeWhile((a: Int) => a < 3).toList == List(1, 2))
    assert(cons(5, cons(2, Empty)).takeWhile((a: Int) => a < 3).toList == Nil)
  }

  test("EXERCISE 5.4 forAll") {
    assert(cons(1, cons(2, Empty)).forAll((a: Int) => a < 3))
    assert(!cons(1, cons(2, Empty)).forAll((a: Int) => a < 2))
  }

  test("Find") {
    assert(cons(1, cons(2, Empty)).find((a: Int) => a < 2).contains(1))
    assert(cons(1, cons(2, Empty)).find((a: Int) => a > 12).isEmpty)
  }

  test("EXERCISE 5.5 takeWhile using foldRight") {
    assert(
      cons(1, cons(2, Empty)).foldRight(Empty: Stream[Int])((a, b) => if (a < 2) cons(a, b) else Empty).toList == List(
        1
      )
    )
    assert(
      cons(1, cons(2, Empty)).foldRight(Empty: Stream[Int])((a, b) => if (a < 3) cons(a, b) else Empty).toList == List(
        1,
        2
      )
    )
    assert(
      cons(5, cons(2, Empty)).foldRight(Empty: Stream[Int])((a, b) => if (a < 3) cons(a, b) else Empty).toList == Nil
    )
  }

  test("EXERCISE 5.7 map filter append flatMap using foldRight") {
    //map
    def map[A, B](l1: Stream[A], z: Stream[B], f: A => B): Stream[B] = l1.foldRight(z)((a, b) => cons(f(a), b))

    assert(map(cons(1, cons(2, Empty)), Empty: Stream[Int], (a: Int) => a + 100).toList == List(101, 102))

    //filter
    assert(
      cons(1, cons(3, cons(2, Empty)))
        .foldRight(Empty: Stream[Int])((a, b) => if (a < 3) cons(a, b) else b)
        .toList == List(1, 2)
    )

    //append
    def append[A](l1: Stream[A], l2: Stream[A]): Stream[A] = l1.foldRight(l2)((a, b) => cons(a, b))

    assert(append(cons(1, cons(3, cons(2, Empty))), cons(-1, cons(-2, Empty))).toList == List(1, 3, 2, -1, -2))

    //flatMap
    def flatMap[A, B](l1: Stream[A], z: Stream[B], f: A => Stream[B]): Stream[B] =
      l1.foldRight(z)((a, b) => append(f(a), b))

    assert(
      flatMap(cons(1, cons(2, Empty)), Empty: Stream[Int], (a: Int) => cons(a + 100, Empty)).toList == List(101, 102)
    )

  }

  import Stream._

  test("EXERCISE 5.8 ones") {
    def ones[A](n: A): Stream[A] = cons(n, ones(n))

    assert(ones(1).take(2).toList == List(1, 1))
  }

  test("EXERCISE 5.9 infinite stream") {

    assert(from(1).take(3).toList == List(1, 2, 3))
  }

  test("EXERCISE 5.10 fibonacci") {

    val fibs: Stream[Int] = {
      def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))

      go(0, 1)
    }

    assert(fibs.take(16).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610))

  }

  test("EXERCISE 5.11 unfold") {

    //even stream
    assert(unfold(2)((a: Int) => Some(a, a + 2)).take(10).toList == List(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))

    //odd stream
    assert(unfold(1)((a: Int) => Some(a, a + 2)).take(10).toList == List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19))

    //even stream with None
    assert(unfold(2)((a: Int) => if (a == 10) None else Some(a, a + 2)).take(10).toList == List(2, 4, 6, 8))

  }

  test("EXERCISE 5.12 fibs constant and ones using unfold") {

    //constant
    def constant[A](a: A): Stream[A] = unfold(a)((a: A) => Some(a, a))

    assert(constant("hi").take(10).toList == List("hi", "hi", "hi", "hi", "hi", "hi", "hi", "hi", "hi", "hi"))

    //ones
    val ones = unfold(1)((a: Int) => Some(a, a))
    assert(ones.take(10).toList == List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

    //fibs
    val fibs: Stream[Int] = unfold((0, 1)) { x =>
      val (value, state) = x
      Some(value, (state, state + value))
    }

    assert(fibs.take(16).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610))

  }

  test("EXERCISE 5.13 map take takeWhile zipWith zipAll using unfold") {

    def map[A, B](l1: Stream[A], f: A => B): Stream[B] = unfold(l1) {
      case Cons(h, t) => Some(f(h()), t())
      case _          => None
    }

    assert(map(cons(1, cons(2, Empty)), (a: Int) => a + 100).toList == List(101, 102))

  }

  /**
    * This doesn't work with standard scala Stream because foldRight is not 'by name'
    *
    * Scala.Stream:  def foldRight[B](z: B)   (f: (A,    B) => B): B
    * Stream:        def foldRight[B](z: => B)(f: (A, => B) => B): B
    */
  ignore("Reading infinite stream") {

    lazy val infiniteStream: Stream[String] = cons("hi", infiniteStream)

    lazy val infiniteStreamStandardScala: scala.LazyList[String] = "hi" #:: infiniteStreamStandardScala

    assert(infiniteStream.foldRight(Empty: Stream[String])(cons(_, _)).take(1).toList == List("hi"))
    assert(infiniteStream.foldRight(Empty: Stream[String])(cons(_, _)).take(2).toList == List("hi", "hi"))
    assert(infiniteStream.foldRight(Empty: Stream[String])(cons(_, _)).take(3).toList == List("hi", "hi", "hi"))

    assertThrows[java.lang.StackOverflowError] {
      infiniteStreamStandardScala.foldRight(scala.LazyList.empty[String])(_ #:: _).take(3).toList
    }
  }
}
