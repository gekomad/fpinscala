import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

class Ch3_Functional_data_structures extends AnyFunSuite {

  object MyList {

    sealed trait List[+A]

    case object Nil extends List[Nothing]

    case class Cons[+A](head: A, tail: List[A]) extends List[A]

    def sum(ints: List[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil         => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

    // EXERCISE 3.10
    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    // EXERCISE 3.14
    def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((a, b) => Cons(a, b))

    // EXERCISE 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] = as match {
      case Nil         => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

    // EXERCISE 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
      case Nil         => Nil
      case Cons(x, xs) => append(f(x), flatMap(xs)(f))
    }
  }

  import MyList._

  test("EXERCISE 3.1") {
    val r: Int = (Cons(1, Cons(2, Cons(3, Cons(4, Nil)))): List[Int]) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <--
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }
    assert(r == 3)
  }

  test("EXERCISE 3.2 tail") {
    def tail[A](l: List[A]): List[A] = l match {
      case Nil         => Nil
      case Cons(_, xs) => xs
    }

    val x: List[Int] = tail(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))): List[Int])
    assert(x == Cons(2, Cons(3, Cons(4, Nil))))
  }

  test("EXERCISE 3.3 setHead") {
    def setHead[A](x: A, l: List[A]): List[A] = l match {
      case Nil         => Nil
      case Cons(_, xs) => Cons(x, xs)
    }

    val x: List[Int] = setHead(1, Cons(2, Cons(2, Cons(3, Nil))))
    assert(x == Cons(1, Cons(2, Cons(3, Nil))))
  }

  test("EXERCISE 3.4 drop") {
    @tailrec
    def drop[A](n: Int, l: List[A]): List[A] = l match {
      case Nil         => Nil
      case Cons(x, xs) => if (n == 0) Cons(x, xs) else drop(n - 1, xs)
    }

    assert(drop(1, Cons(0, Cons(1, Cons(2, Nil)))) == Cons(1, Cons(2, Nil)))
  }

  test("EXERCISE 3.5 dropWhile") {
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil         => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    assert(dropWhile(Cons(0, Cons(1, Cons(2, Nil))), (a: Int) => a < 2) == Cons(0, Cons(1, Nil)))
  }

  test("EXERCISE 3.6 init") {
    def init[A](l: List[A]): List[A] = l match {
      case Nil          => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs)  => Cons(x, init(xs))
    }

    assert(init(Cons(0, Cons(1, Cons(2, Nil)))) == Cons(0, Cons(1, Nil)))
  }

  test("EXERCISE 3.8 foldRight") {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    val x = foldRight(l, Nil: List[Int])(Cons(_, _))
    assert(x == l)
  }

  test("EXERCISE 3.9 length") {
    def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

    assert(length(Cons(1, Cons(2, Cons(3, Nil)))) == 3)
  }

  test("EXERCISE 3.10 foldLeft") {

    val l = Cons(1, Cons(2, Cons(3, Nil)))

    assert(foldLeft(l, 0)(_ + _) == 6)
  }

  test("EXERCISE 3.11 apply foldLeft") {

    def sum(ns: List[Int]): Int = foldLeft(ns, 0)((x, y) => x + y)

    def product(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

    val li = Cons(1, Cons(2, Cons(3, Nil)))
    val ld = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))

    assert(sum(li) == 6)
    assert(product(ld) == 6)

    def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

    assert(length(Cons(1, Cons(2, Cons(3, Nil)))) == 3)
  }

  test("EXERCISE 3.12 reverse") {
    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((a, b) => Cons(b, a))

    val l = Cons(1, Cons(2, Cons(3, Nil)))

    assert(reverse(l) == Cons(3, Cons(2, Cons(1, Nil))))
  }

  test("EXERCISE 3.14 append") {

    val l1   = Cons(1, Cons(2, Cons(3, Nil)))
    val l2   = Cons(4, Cons(5, Cons(6, Nil)))
    val l1l2 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))

    assert(append(l1, l2) == l1l2)

  }

  test("EXERCISE 3.16 add 1") {

    def plus1(l: List[Int]): List[Int] = l match {
      case Nil         => Nil
      case Cons(x, xs) => Cons(x + 1, plus1(xs))
    }

    assert(plus1(Cons(1, Cons(2, Cons(3, Nil)))) == Cons(2, Cons(3, Cons(4, Nil))))

  }

  test("EXERCISE 3.17 to string") {

    def toString(l: List[Double]): List[String] = l match {
      case Nil         => Nil
      case Cons(x, xs) => Cons(x.toString, toString(xs))
    }

    assert(toString(Cons(1, Cons(2, Cons(3, Nil)))) == Cons("1.0", Cons("2.0", Cons("3.0", Nil))))

  }

  test("EXERCISE 3.18 map") {

    assert(map(Cons(1, Cons(2, Cons(3, Nil))))(_ + 1) == Cons(2, Cons(3, Cons(4, Nil))))
    assert(map(Cons(1, Cons(2, Cons(3, Nil))): List[Double])(_.toString) == Cons("1.0", Cons("2.0", Cons("3.0", Nil))))

  }

  test("EXERCISE 3.19 filter") {

    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Nil         => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
    }

    assert(filter(Cons(1, Cons(2, Cons(3, Nil))))(_ > 1) == Cons(2, Cons(3, Nil)))

  }

  test("EXERCISE 3.20 flatMap") {

    assert(flatMap(Cons(1, Cons(2, Nil)))(i => Cons(i, Cons(i, Nil))) == Cons(1, Cons(1, Cons(2, Cons(2, Nil)))))

  }

  test("EXERCISE 3.21 filter with flatMap") {

    assert(flatMap(Cons(1, Cons(2, Cons(3, Nil))))(a => if (a > 1) Cons(a, Nil) else Nil) == Cons(2, Cons(3, Nil)))

  }

  test("EXERCISE 3.22 adding pair") {

    def sum(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _)                       => Nil
      case (_, Nil)                       => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, sum(xs1, xs2))

    }

    assert(sum(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Nil)))) == Cons(5, Cons(7, Cons(9, Nil))))

  }

  test("EXERCISE 3.23 zipWith") {

    def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
      case (Nil, _)                       => Nil
      case (_, Nil)                       => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))

    }

    assert(
      zipWith(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Nil))))(_ + _) == Cons(5, Cons(7, Cons(9, Nil)))
    )

  }

  object MyTree {

    sealed trait Tree[+A]

    case class Leaf[A](value: A) extends Tree[A]

    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  }

  import MyTree._

  test("EXERCISE 3.25 Tree size") {

    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    assert(size(Leaf(1)) == 1)
    assert(size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(size(Branch(Leaf(1), Branch(Leaf(3), Leaf(4)))) == 5)

  }

  test("EXERCISE 3.26 Tree[Int] maximum leaf") {

    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(n)      => n
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

    assert(maximum(Leaf(1)) == 1)
    assert(maximum(Branch(Leaf(1), Leaf(2))) == 2)
    assert(maximum(Branch(Leaf(1), Branch(Leaf(3), Leaf(4)))) == 4)

  }

  test("EXERCISE 3.27 Tree max depth") {

    def depth[A](t: Tree[A], d: Int = 1): Int = t match {
      case Leaf(_)      => d
      case Branch(l, r) => depth(l, d + 1) max depth(r, d + 1)
    }

    assert(depth(Leaf(1)) == 1)
    assert(depth(Branch(Leaf(1), Leaf(2))) == 2)
    assert(depth(Branch(Leaf(1), Branch(Leaf(3), Leaf(4)))) == 3)
    assert(depth(Branch(Leaf(1), Branch(Leaf(3), Branch(Leaf(3), Leaf(4))))) == 4)
  }

  test("EXERCISE 3.28 Tree map") {

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(x)      => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    assert(map(Leaf(1))(_ + 1) == Leaf(2))
    assert(map(Branch(Leaf(1), Leaf(2)))(_ + 1) == Branch(Leaf(2), Leaf(3)))
    assert(map(Branch(Leaf(1), Branch(Leaf(3), Leaf(4))))(_ + 1) == Branch(Leaf(2), Branch(Leaf(4), Leaf(5))))

  }

  test("EXERCISE 3.29 Tree fold") {

    def fold[A, B](f: A => B)(g: (B, B) => B)(t: Tree[A]): B = t match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(fold(f)(g)(l), fold(f)(g)(r))
    }
    {
      // sum 1 and max

      def sumAndMax: Tree[Int] => Int = fold((a: Int) => a + 1)((a: Int, b: Int) => a max b)

      assert(sumAndMax(Leaf(1)) == 2)
      assert(sumAndMax(Branch(Leaf(1), Leaf(2))) == 3)
      assert(sumAndMax(Branch(Leaf(1), Branch(Leaf(3), Leaf(4)))) == 5)
    }

    {
      // size (branches + leafs)

      def size: Tree[Int] => Int = fold((_: Int) => 1)((a: Int, b: Int) => a + b + 1)

      assert(size(Leaf(1)) == 1)
      assert(size(Branch(Leaf(1), Leaf(2))) == 3)
      assert(size(Branch(Leaf(1), Branch(Leaf(3), Leaf(4)))) == 5)
    }

    {
      // max depth

      def depth: Tree[Int] => Int = fold((_: Int) => 1)((a: Int, b: Int) => a + b)

      assert(depth(Leaf(1)) == 1)
      assert(depth(Branch(Leaf(1), Leaf(2))) == 2)
      assert(depth(Branch(Leaf(1), Branch(Leaf(3), Leaf(4)))) == 3)
      assert(depth(Branch(Leaf(1), Branch(Leaf(3), Branch(Leaf(3), Leaf(4))))) == 4)
    }
  }

}
