import org.scalatest.FunSuite


trait Applicative[F[_]] extends Functor[F] {
  //two methods are going to use the default definition, and two (unit and one among map2/apply) need to be overridden
  self =>
  //unit and map2 are primitives
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  //EXERCISE 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] = fas.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = map2(fa, unit(())) { (m, _) => List.fill(n)(m) }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb) { (m, h) => (m, h) }

  //EXERCISE 12.2

  def unit[A](a: => A): F[A]

  //Define in terms of map2 and unit
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, fb) => fb(a))

  //Define in terms of apply and unit
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply {
      val x: F[B => C] = map(fa) {
        val o: A => B => C = f.curried
        o
      }
      x
    }(fb)

  //EXERCISE 12.3
  // Define in terms of  unit, apply, and the curried

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val o1 = apply(unit(f.curried))(fa)
    val o2 = apply(o1)(fb)
    val o3 = apply(o2)(fc)
    o3
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val o1 = apply(unit(f.curried))(fa)
    val o2 = apply(o1)(fb)
    val o3 = apply(o2)(fc)
    val o4 = apply(o3)(fd)
    o4
  }

  //EXERCISE 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldRight(unit(Map.empty[K, V])) {
    case ((k, fv), acc) => {
      map2(fv, acc) { (a, b) =>
        b + (k -> a)
      }
    }
  }

}

sealed trait Validation[+E, +A]

final case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

final case class Success[A](a: A) extends Validation[Nothing, A]


trait Traverse[F[_]] extends Functor[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)
}

class Ch12_Applicative_and_traversable_functors extends FunSuite {

  test("EXERCISE 12.3") {
    val app = new Applicative[Option] {
      override def unit[A](a: => A): Option[A] = Some(a)

      override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = fa.flatMap(a => fb.map(r => f(a, r)))

    }
    // def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, fb) => fb(a))
    val p1 = app.apply(Some((a: Int) => a + 1))(Some(1))
    val p2 = app.map3(Some(1), Some(2), Some(3))((a: Int, b: Int, c: Int) => a + b + c)

  }


  test("EXERCISE 12.5") {

    def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
      new Monad[({type f[x] = Either[E, x]})#f] {
        def unit[A](a: => A): Either[E, A] = Right(a)

        def flatMap[A, B](ma: Either[E, A])(ff: A => Either[E, B]): Either[E, B] = ma.flatMap(ff)

      }


    def f: Double => Either[String, Int] = (a: Double) => if (a.toInt % 2 == 0) Right(a.toInt + 100) else Left(s"ko $a")

    val e = eitherMonad[String]

    // flatMap[Double, Int](ma: Either[String, Double])(f: Double => Either[String, Int]): Either[String, Int]
    assert(e.flatMap(Right(1.0))(f) == Left("ko 1.0"))
    assert(e.flatMap(Right(2.0))(f) == Right(102))

    assert(e.flatMap(Left("err"))(f) == Left("err"))

  }

  test("EXERCISE 12.6 validate") {

    def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
      new Applicative[({type f[x] = Validation[E, x]})#f] {
        def unit[A](a: => A): Validation[Nothing, A] = Success(a)

        override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
          case (Failure(h1, t1), Failure(h2, t2)) =>
            Failure(h1, (h2 +: t2) ++ t1)

          case (_, a@Failure(_, _)) =>
            a

          case (a@Failure(_, _), _) =>
            a

          case (Success(a), Success(b)) =>
            Success(f(a, b))
        }
      }


    import java.util.Date

    case class WebForm(name: String, birthdate: Date, phoneNumber: String)

    def validName(name: String): Validation[String, String] =
      if (name != "") Success(name)
      else Failure("Name cannot be empty")

    def validBirthdate(birthdate: String): Validation[String, Date] = try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case _: java.text.ParseException =>
        Failure("Birthdate must be in the form yyyy-MM-dd", Vector())
    }

    def validPhone(phoneNumber: String): Validation[String, String] = if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber) else Failure("Phone number must be 10 digits")

    val nameBirthday = (a: String, b: Date) => s"name: $a birthday: $b"
    val nameBirthdayPhone = (a: String, b: String) => s"$a phone: $b"

    {
      val name = validName("")
      val birthDate = validBirthdate("")
      val phone = validPhone("")

      val p1 = validationApplicative[String].map2(name, birthDate)(nameBirthday)
      val p = validationApplicative[String].map2(p1, phone)(nameBirthdayPhone)
      assert(p == Failure("Name cannot be empty", Vector("Phone number must be 10 digits", "Birthdate must be in the form yyyy-MM-dd")))
    }

    {
      val name = validName("Bob")
      val birthDate = validBirthdate("")
      val phone = validPhone("")

      val p1 = validationApplicative[String].map2(name, birthDate)(nameBirthday)
      val p = validationApplicative[String].map2(p1, phone)(nameBirthdayPhone)
      assert(p == Failure("Birthdate must be in the form yyyy-MM-dd", Vector("Phone number must be 10 digits")))
    }

    {
      val name = validName("")
      val birthDate = validBirthdate("1980-01-01")
      val phone = validPhone("")

      val p1 = validationApplicative[String].map2(name, birthDate)(nameBirthday)
      val p = validationApplicative[String].map2(p1, phone)(nameBirthdayPhone)
      assert(p == Failure("Name cannot be empty", Vector("Phone number must be 10 digits")))
    }

    {
      val name = validName("")
      val birthDate = validBirthdate("")
      val phone = validPhone("0123456789")

      val p1 = validationApplicative[String].map2(name, birthDate)(nameBirthday)
      val p = validationApplicative[String].map2(p1, phone)(nameBirthdayPhone)
      assert(p == Failure("Name cannot be empty", Vector("Birthdate must be in the form yyyy-MM-dd")))
    }

    {
      val name = validName("")
      val birthDate = validBirthdate("1980-01-01")
      val phone = validPhone("0123456789")

      val p1 = validationApplicative[String].map2(name, birthDate)(nameBirthday)
      val p = validationApplicative[String].map2(p1, phone)(nameBirthdayPhone)
      assert(p == Failure("Name cannot be empty", Vector()))
    }

    {
      val name = validName("Bob")
      val birthDate = validBirthdate("1980-01-01")
      val phone = validPhone("")

      val p1 = validationApplicative[String].map2(name, birthDate)(nameBirthday)
      val p = validationApplicative[String].map2(p1, phone)(nameBirthdayPhone)
      assert(p == Failure("Phone number must be 10 digits", Vector()))
    }

    {
      val name = validName("Bob")
      val birthDate = validBirthdate("1980-01-01")
      val phone = validPhone("0123456789")

      val p1 = validationApplicative[String].map2(name, birthDate)(nameBirthday)
      val p = validationApplicative[String].map2(p1, phone)(nameBirthdayPhone)
      assert(p == Success("name: Bob birthday: Tue Jan 01 00:00:00 CET 1980 phone: 0123456789"))
    }
  }

  test("EXERCISE 12.12 sequence") {
    val app = new Applicative[Option] {
      override def unit[A](a: => A): Option[A] = Some(a)

      override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = fa.flatMap(a => fb.map(r => f(a, r)))
    }

    assert {
      app.sequenceMap(Map(1 -> Some(101.0), 2 -> Some(102.0))) == Some(Map(1 -> 101.0, 2 -> 102.0))
    }

    assert {
      app.sequenceMap(Map(1 -> Some(101.0), 2 -> None)) == None
    }

  }

  test("EXERCISE 12.13") {
    /*
    trait Traverse[F[_]] extends Functor[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)
  }

    * */
    val tr= new Traverse[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = ???
    }
  }
}
