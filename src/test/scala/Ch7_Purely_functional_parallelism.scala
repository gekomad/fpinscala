import java.util.concurrent._

import org.scalatest.FunSuite

class Ch7_Purely_functional_parallelism extends FunSuite {

  //EXERCISE 7.2 Par
  type Par[A] = ExecutorService => Future[A]

  object Par {

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit): A = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af: Future[A] = a(es)
        val bf: Future[B] = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call: A = a(es).get
      })

    def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

    def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => Par.lazyUnit(f(a))

    // EXERCISE 7.5 sequence
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight[Par[List[A]]](Par.unit(List.empty[A]))((h, t) => Par.map2(h, t)(_ :: _))
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  }

  test("EXERCISE 7.2 evaluate function A => B asynchronously") {

    def f: Int => String = (a: Int) => s"[${a.toString}]"

    val p: Int => Par[String] = Par.asyncF(f)

    val p3 = Par.run(Executors.newFixedThreadPool(2))(p(1)).get

    assert(p3 == "[1]")

  }

  test("sortPar using map") {

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = Par.map(parList)(_.sorted)

    assert(Par.run(Executors.newFixedThreadPool(2))(sortPar(Par.unit(List(1, 3, 2)))).get == List(1, 2, 3))
  }

  test("EXERCISE 7.5 sequence") {

    assert {
      Par.run(Executors.newFixedThreadPool(2))(Par.sequence(List(Par.unit(1), Par.unit(2)))).get == List(1, 2)
    }
  }

  test("parMap") {
    assert(Par.run(Executors.newFixedThreadPool(2))(Par.parMap(List(1, 3, 2))((a: Int) => a + 100)).get == List(101, 103, 102))
  }

  test("EXERCISE 7.6 par filter") {

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = as match {
      case Nil => Par.unit(Nil)
      case x :: xs => if (f(x)) Par.map2(Par.unit(x), parFilter(xs)(f))(_ :: _) else parFilter(xs)(f)
    }

    val l = (0 to 100).toList

    assert(Par.run(Executors.newFixedThreadPool(2))(parFilter(l)((a: Int) => a % 2 == 0)).get.length == 51)

  }

  test("count words in paragraphs") {

    def parGen[A, B](z: B)(as: List[A])(f: A => B)(g: (B, B) => B): Par[B] = as match {
      case Nil => Par.unit(z)
      case x :: xs => Par.map2(Par.unit(f(x)), parGen(z)(xs)(f)(g))(g(_, _))
    }

    val paragraphs = List("aaa bb cc", "dd ee ff")

    def countWords: String => Int = (a: String) => a.split(" ").length

    assert(Par.run(Executors.newFixedThreadPool(2))(parGen(0)(paragraphs)(countWords)(_ + _)).get == 6)

  }

  test("parallel summation") {

    def sum(ints: IndexedSeq[Int]): Par[Int] = {

      def sumList(ints: List[Int]): Par[Int] = ints match {
        case Nil => Par.unit(0)
        case x :: xs => Par.map2(Par.unit(x), sumList(xs))(_ + _)
      }

      sumList(ints.toList)
    }

    assert(Par.run(Executors.newFixedThreadPool(2))(sum(1 to 3)).get == 6)
  }
}
