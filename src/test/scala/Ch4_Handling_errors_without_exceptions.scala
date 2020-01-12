import org.scalatest.funsuite.AnyFunSuite

class Ch4_Handling_errors_without_exceptions extends AnyFunSuite {

  object MyOption {

    // EXERCISE 4.1
    sealed trait Option[+A] {
      def map[B](f: A => B): Option[B] = this match {
        case None    => None
        case Some(a) => Some(f(a))
      }

      def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None    => None
        case Some(a) => f(a)
      }

      def getOrElse[B >: A](default: => B): B = this match {
        case None    => default
        case Some(a) => a
      }

      def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case a    => a
      }

      def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) if f(a) => Some(a)
        case _               => None
      }
    }

    case class Some[+A](get: A) extends Option[A]

    case object None extends Option[Nothing]

  }

  import MyOption._

  test("EXERCISE 4.2 variance") {
    def variance(xs: Seq[Double]): Option[Double] = {

      def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.size)

      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    assert(variance(List()) == None)
    assert(variance(List(1, 1, 1)) == Some(0))
    assert(variance(List(12.0, 12.0, 12.0, 12.0, 13.0)) == Some(0.16))
    assert(variance(List(12, 12, 12, 12, 13013)) == Some(2.7044160159999996E7))
    assert(variance(List(600, 470, 170, 430, 300)) == Some(21704.0))
  }

  test("EXERCISE 4.3 map2") {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(a1 => b.map(b1 => f(a1, b1)))

    def f(a: Int, b: Int): Int = a + b

    assert(map2(Some(1), Some(2))(f) == Some(3))
    assert(map2(Some(1), None)(f) == None)
    assert(map2(None, None)(f) == None)
    assert(map2(None, Some(1))(f) == None)
  }

  test("EXERCISE 4.4 sequence") {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case x :: xs =>
        x flatMap { x1 =>
          val s: Option[List[A]] = sequence(xs)
          val p: Option[List[A]] = s.map(xss => x1 :: xss)
          p
        }
    }

    assert(sequence(List(None)) == None)
    assert(sequence(List(None, Some(1))) == None)
    assert(sequence(List(Some(1))) == Some(List(1)))
    assert(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))

  }

  object MyEither {

    // EXERCISE 4.6
    sealed trait Either[+E, +A] {
      def map[B](f: A => B): Either[E, B] = this match {
        case Left(a)  => Left(a)
        case Right(a) => Right(f(a))
      }

      def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(a)  => Left(a)
        case Right(a) => f(a)
      }

      def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(_)  => b
        case Right(a) => Right(a)
      }

      def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap { a1 =>
        b.map(b1 => f(a1, b1))
      }

      def map2bis[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = (this, b) match {
        case (Right(a1), Right(b1)) => Right(f(a1, b1))
        case (Left(a1), Left(b1))   => Left(List(a1, b1))
        case (Left(a1), _)          => Left(List(a1))
        case (_, Left(b1))          => Left(List(b1))
      }

    }

    case class Left[+E](value: E) extends Either[E, Nothing]

    case class Right[+A](value: A) extends Either[Nothing, A]

  }

  import MyEither._

  test("EXERCISE 4.7 sequence and traverse") {

    { //sequence
      def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
        case Nil => Right(List.empty[A])

        case x :: xs =>
          x flatMap { x1 =>
            val s = sequence(xs)
            val p = s.map(xss => x1 :: xss)
            p
          }
      }

      assert(sequence(List(Left(1))) == Left(1))
      assert(sequence(List(Left(1), Right(1))) == Left(1))
      assert(sequence(List(Right(1))) == Right(List(1)))
      assert(sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
    }

    {
      //traverse
      def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
        case Nil => Right(List.empty[B])

        case x :: xs =>
          f(x) flatMap { x1 =>
            val s = traverse(xs)(f)
            val p = s.map(xss => x1 :: xss)
            p
          }
      }

      def f(a: Int): MyEither.Either[String, Int] = if (a == 0) Left(s"err $a") else Right(a + 1)

      assert(traverse(List(1))(f) == Right(List(2)))
      assert(traverse(List(1, 0))(f) == Left("err 0"))
      assert(traverse(List(1, 2))(f) == Right(List(2, 3)))

    }
  }

  test("EXERCISE 4.8 map2bis") {
    case class Person(name: Name, age: Age)
    sealed case class Name(value: String)
    sealed case class Age(value: Int)

    def mkName(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is empty.")
      else Right(Name(name))

    def mkAge(age: Int): Either[String, Age] =
      if (age < 0) Left("Age is out of range.")
      else Right(Age(age))

    def mkPerson(name: String, age: Int): Either[List[String], Person] =
      mkName(name).map2bis(mkAge(age))(Person)

    assert(mkPerson("Bob", 10) == Right(Person(Name("Bob"), Age(10))))
    assert(mkPerson("", 10) == Left(List("Name is empty.")))
    assert(mkPerson("Bob", -1) == Left(List("Age is out of range.")))
    assert(mkPerson("", -1) == Left(List("Name is empty.", "Age is out of range.")))

  }

}
