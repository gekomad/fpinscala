import org.scalatest.FunSuite

object Ch14 {

  sealed trait ST[S, A] {
    self =>
    protected def run(s: S): (A, S)

    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }

    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  object ST {
    def apply[S, A](a: => A): ST[S, A] = {
      lazy val memo = a
      new ST[S, A] {
        def run(s: S): (A, S) = (memo, s)
      }
    }

    def runST[A](st: RunnableST[A]): A =
      st.apply[Unit].run(())._1
  }

  //////////////

  sealed trait STRef[S, A] {
    protected var cell: A

    def read: ST[S, A] = ST(cell)

    def write(a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        cell = a
        ((), s)
      }
    }
  }

  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
      var cell: A = a
    })
  }

  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

}

class Ch14_Local_effects_and_mutable_state extends FunSuite {

  test("runST") {
    import Ch14._
    import Ch14.ST._

    val p = new RunnableST[(Int, Int)] {
      def apply[S]: ST[S, (Int, Int)] = for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1) // 2 + 1
        _ <- r2.write(x + 1) // 1 + 1
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }

    val sp = runST(p)
    assert(sp == (3, 2))
  }
}
