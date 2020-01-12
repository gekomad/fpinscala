import org.scalatest.funsuite.AnyFunSuite

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // EXERCISE 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List.empty[A]))((cur, acc) => map2(cur, acc)(_ :: _))
    //    lma.foldRight(unit(List.empty[A]))((cur, acc) => {
    //      flatMap(acc) { b3 => flatMap(cur)(v => unit(v :: b3))
    //      }
    //    })

  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List.empty[B]))((cur, acc) => map2(f(cur), acc)(_ :: _))
    //    la.foldRight(unit(List.empty[B]))((a, b2) => {
    //      flatMap(b2) { b3 => flatMap(f(a))(v => unit(v :: b3))
    //      }
    //    })
  }

  // EXERCISE 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = map(ma) { m =>
    List.fill(n)(m)
  }

  // EXERCISE 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List.empty[A])) { (cur, acc) =>
      flatMap(acc) { macc =>
        map(f(cur)) { b =>
          if (b) cur :: macc else macc
        }
      }
    }
  }

  // EXERCISE 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    //aa => flatMap(unit(aa)) { a => flatMap(f(a))(g) } --> by identity laws: flatMap(unit(aa)) == aa
    aa => flatMap(f(aa))(g)
}

object MyMonad {
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f

  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }
}

import MyMonad._

class Ch11_Monads extends AnyFunSuite {
  test("EXERCISE 11.1 Monad[List]") {

    //listMonad
    {
      assert(listMonad.flatMap(List(1, 2, 3))(a => List(a + 100)) == List(101, 102, 103))
      assert(listMonad.flatMap(List.empty[Int])(a => List(a + 100)) == List())
      assert(listMonad.unit(1) == List(1))
    }

    //listOption
    {
      assert(optionMonad.flatMap(Some(1))(a => Some(a + 100)) == Some(101))
      assert(optionMonad.flatMap(None: Option[Int])(a => Some(a + 100)) == None)
      assert(optionMonad.unit(1) == Some(1))
    }
  }

  test("EXERCISE 11.3 sequence and traverse") {
    val l: List[Option[Int]]     = List(optionMonad.unit(1), optionMonad.unit(2))
    val empty: List[Option[Int]] = List.empty[Option[Int]]

    assert(optionMonad.sequence(l) == Some(List(1, 2)))

    assert(optionMonad.sequence(empty) == Some(List()))

  }
  test("EXERCISE 11.4 replicateM") {
    assert(optionMonad.replicateM(5, Some(1)) == Some(List(1, 1, 1, 1, 1)))
    assert(listMonad.replicateM(5, List(1)) == List(List(1, 1, 1, 1, 1)))
  }

  test("EXERCISE 11.7 Kleisli composition") {

    def f: Int => Option[String] = (a: Int) => if (a == 0) None else Some((a + 1).toString)

    def g: String => Option[Double] = (a: String) => if (a.toInt % 2 == 0) Some(a.toDouble) else None

    assert(optionMonad.compose(f, g)(0) == None)
    assert(optionMonad.compose(f, g)(1) == Some(2.0))
    assert(optionMonad.compose(f, g)(3) == Some(4.0))
    assert(optionMonad.compose(f, g)(4) == None)

    {
      def h: Double => Option[Double] = (a: Double) => if (a.toInt % 2 != 0) Some(a) else None

      assert {
        optionMonad.compose(optionMonad.compose(f, g), h)(1) == optionMonad.compose(f, optionMonad.compose(g, h))(1)
      }
    }
  }

  test("EXERCISE 11.11 identity laws") {

    {
      //    flatMap(x)(unit) == x
      val x = 2
      assert(optionMonad.flatMap(Some(x))(optionMonad.unit(_)) == Some(x))
    }

    val f: Int => Option[Int] = (a: Int) => if (a == 0) None else Some(a + 1)

    {
      //    flatMap(unit(x))(f) == f(x)
      val x = 2
      assert(f(x) == Some(3))
      assert(optionMonad.flatMap(optionMonad.unit(x))(f) == f(x))
    }

    {
      val x = 0
      assert(f(x) == None)
      assert(optionMonad.flatMap(optionMonad.unit(x))(f) == f(x))
    }
  }

  test("EXERCISE 11.17") {
    case class Id[A](value: A) {

      def map[B](f: A => B): Id[B] = Id(f(value))

      def flatMap[B](f: A => Id[B]): Id[B] = f(value)
    }

    val a = new Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)

      override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
    }

    assert(a.map(Id(1))(_ + 1) == Id(2))
  }
}
