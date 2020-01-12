import org.scalatest.funsuite.AnyFunSuite

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

import Monoid._

class Ch10_Monoids extends AnyFunSuite {

  //EXERCISE 10.5 foldMap
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((a, b) => m.op(a, f(b)))

  test("stringMonoid") {
    val words = List("Hic", "Est", "Index")
    assert("HicEstIndex" == words.foldRight(stringMonoid.zero)(stringMonoid.op))
    assert("HicEstIndex" == words.foldLeft(stringMonoid.zero)(stringMonoid.op))
  }

  test("EXERCISE 10.5 foldMap") {

    val d = foldMap(List(1, 2, 3), stringMonoid)("|" + _)
    assert(d == "|1|2|3")
  }

  test("EXERCISE 10.7 foldMap for IndexedSeq") {
    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
      case 0 => m.zero
      case 1 => m.op(m.zero, f(v(0)))
      case _ =>
        val (a, b) = v.splitAt(v.length / 2)
        val l: B   = foldMapV(a, m)(f)
        val r: B   = foldMapV(b, m)(f)
        m.op(l, r)
    }

    val d = foldMapV(List(1, 2, 3).toVector, stringMonoid)("|" + _)
    assert(d == "|1|2|3")
  }

  test("EXERCISE 10.8 parFoldMap for IndexedSeq using scala Future") {
    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Future[B] = v.length match {
      case 0 => Future.successful(m.zero)
      case 1 => Future.successful(m.op(m.zero, f(v(0))))
      case _ =>
        val (a, b)       = v.splitAt(v.length / 2)
        val l: Future[B] = parFoldMap(a, m)(f)
        val r: Future[B] = parFoldMap(b, m)(f)

        for {
          a <- l
          b <- r
        } yield m.op(a, b)
    }

    val d: Future[String] = parFoldMap(List(1, 2, 3).toVector, stringMonoid)("|" + _)

    val res = Await.result(d, Duration.Inf)
    assert(res == "|1|2|3")
  }

  test("Listing 10.1") {
    def mapMergeMonoid[K, V](m: Monoid[V]): Monoid[Map[K, V]] =
      new Monoid[Map[K, V]] {
        def zero: Map[K, V] = Map[K, V]()

        def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
          (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
            acc.updated(k, m.op(a.getOrElse(k, m.zero), b.getOrElse(k, m.zero)))
          }
      }

    val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))

    val m1: Map[String, Map[String, Int]] = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val m2: Map[String, Map[String, Int]] = Map("o1" -> Map("i2" -> 3))
    val m3: Map[String, Map[String, Int]] = M.op(m1, m2)

    assert(m3 == Map("o1" -> Map("i1" -> 1, "i2" -> 5)))

  }

}

object Monoid {
  // EXERCISE 10.1 Monoid[Int] Monoid[Boolean]
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  // EXERCISE 10.2 Monoid[Option[A]]
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None

  }

  val optionInt: Monoid[Option[Int]] = optionMonoid[Int]

  // EXERCISE 10.3 endoMonoid[A]
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2

    val zero: A => A = (a: A) => a
  }

  val endoMonoidInt: Monoid[Int => Int] = endoMonoid[Int]

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  //EXERCISE 10.16
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (ma.op(a1._1, a2._1), mb.op(a1._2, a2._2))

    val zero: (A, B) = (ma.zero, mb.zero)
  }

  val productMonoidInt: Monoid[(Int, Int)] = productMonoid(intAddition, intAddition)
}

// EXERCISE 10.4
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Monoid._

object StringSpecification extends Properties("Monoid") {

  //intAddition
  property("[Int] scalaCheck: addition identityLaw") = forAll { a: Int =>
    intAddition.op(intAddition.zero, a) == intAddition.op(a, intAddition.zero)
  }

  property("[Int] scalaCheck: addition associativityLaw") = forAll { (x: Int, y: Int, z: Int) =>
    intAddition.op(intAddition.op(x, y), z) == intAddition.op(x, intAddition.op(y, z))
  }

  //intMultiplication
  property("[Int] scalaCheck: multiplication identityLaw") = forAll { a: Int =>
    intMultiplication.op(intMultiplication.zero, a) == intMultiplication.op(a, intMultiplication.zero)
  }

  property("[Int] scalaCheck: multiplication associativityLaw") = forAll { (x: Int, y: Int, z: Int) =>
    intMultiplication.op(intMultiplication.op(x, y), z) == intMultiplication.op(x, intMultiplication.op(y, z))
  }

  //booleanOr
  property("[Boolean] scalaCheck: booleanOr identityLaw") = forAll { a: Boolean =>
    booleanOr.op(booleanOr.zero, a) == booleanOr.op(a, booleanOr.zero)
  }

  property("[Boolean] scalaCheck: booleanOr associativityLaw") = forAll { (x: Boolean, y: Boolean, z: Boolean) =>
    booleanOr.op(booleanOr.op(x, y), z) == booleanOr.op(x, booleanOr.op(y, z))
  }

  //booleanAnd
  property("[Boolean] scalaCheck: booleanAnd identityLaw") = forAll { a: Boolean =>
    booleanAnd.op(booleanAnd.zero, a) == booleanAnd.op(a, booleanAnd.zero)
  }

  property("[Boolean] scalaCheck: booleanAnd associativityLaw") = forAll { (x: Boolean, y: Boolean, z: Boolean) =>
    booleanAnd.op(booleanAnd.op(x, y), z) == booleanAnd.op(x, booleanAnd.op(y, z))
  }

  //Option
  property("[Option[Int]] scalaCheck: option identityLaw") = forAll { a: Option[Int] =>
    optionInt.op(optionInt.zero, a) == optionInt.op(a, optionInt.zero)
  }

  property("[Option[Int]] scalaCheck: option associativityLaw") = forAll {
    (x: Option[Int], y: Option[Int], z: Option[Int]) =>
      optionInt.op(optionInt.op(x, y), z) == optionInt.op(x, optionInt.op(y, z))
  }

  //endoMonoid
  property("endoMonoidInt scalaCheck: endoMonoid identityLaw") = forAll { (a: Int => Int, b: Int) =>
    endoMonoidInt.op(endoMonoidInt.zero, a)(b) == endoMonoidInt.op(a, endoMonoidInt.zero)(b)
  }

  property("endoMonoidInt scalaCheck: endoMonoid associativityLaw") = forAll { (x: Int => Int, y: Int => Int, z: Int => Int, b: Int) =>
    endoMonoidInt.op(endoMonoidInt.op(x, y), z)(b) == endoMonoidInt.op(x, endoMonoidInt.op(y, z))(b)
  }

  //productMonoidInt EXERCISE 10.16
  property("[productMonoid[Int,Int]] scalaCheck: productMonoidInt identityLaw") = forAll { a: (Int, Int) =>
    productMonoidInt.op(productMonoidInt.zero, a) == productMonoidInt.op(a, productMonoidInt.zero)
  }
}
