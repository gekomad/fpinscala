import scala.language.implicitConversions
import ch8._
import ch8.Prop.Prop
import ch8.Prop._
import org.scalatest.funsuite.AnyFunSuite

import scala.util.matching.Regex

class Ch9_Parser_combinators extends AnyFunSuite {

  trait Parsers[ParseError, Parser[+ _]] {
    self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= 0) succeed(List())
      else map2(p, listOfN(n - 1, p))(_ :: _)

    //Always succeeds with the value a
    def succeed[A](a: A): Parser[A] = map(string(""))(_ => a)

    //Chooses between two parsers, first attempting p1 , and then p2 if p1 fails
    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

    //EXERCISE 9.8
    //Applies the function f to the result of p , if successful
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(a => succeed(f(a)))

    //Recognizes and returns a single String
    implicit def string(s: String): Parser[String]

    //Runs a parser, then uses its result to select a second parser to run in sequence
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    implicit def regex(r: Regex): Parser[String]

    def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

    //    def EXERCISE_9_6 = for {
    //      digit <- "[0-9]+".r
    //      // digit <- "0" | "1" |"2" | "3" |"4" | "5" |"6" | "7" |"8" | "9"
    //      val n = regex(digit).toInt
    //      // we really should catch exceptions thrown by toInt
    //      // and convert to parse failure
    //      _ <- listOfN(n, char('a'))
    //    } yield n

    //EXERCISE 9.7
    //Implement product and map2 in terms of flatMap
    def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      flatMap(p1)(a => flatMap(p2)(b => succeed((a, b))))

    //EXERCISE 9.7
    //Implement product and map2 in terms of flatMap
    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = flatMap(p) { a =>
      map(p2) { b =>
        f(a, b)
      }
    }

    //Returns the portion of input inspected by p if successful
    def slice[A](p: Parser[A]): Parser[String]

    def map2Bis[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
      val i: Parser[(A, B)] = product(p, p2)
      val ff: ((A, B)) => C = f.tupled
      val ii: Parser[C] = map(i)(ff)
      ii
    }

    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

    def many1[A](p: Parser[A]): Parser[List[A]] = {
      val x: Parser[List[A]] = many(p)

      val f: (A, List[A]) => List[A] = (a: A, b: List[A]) => a :: b

      map2(p, x)(f)
    }

    trait Parsers[ParseError, Parser[+ _]] {
      self =>
      def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

      implicit def string(s: String): Parser[String]

      implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

      implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

      case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      }

    }

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

      def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

      def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

      def many[B >: A]: Parser[List[B]] = self.many(p)

      def slice: Parser[String] = self.slice(p)

    }

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        forAll(in)(s => run(p1)(s) == run(p2)(s))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, map(p)(a => a))(in)
    }

  }

  // EXERCISE 9.9
  /* trait JSON

   object JSON {

     case object JNull extends JSON

     case class JNumber(get: Double) extends JSON

     case class JString(get: String) extends JSON

     case class JBool(get: Boolean) extends JSON

     case class JArray(get: IndexedSeq[JSON]) extends JSON

     case class JObject(get: Map[String, JSON]) extends JSON

     def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
       import P._
       implicit def tok(s: String) = token(P.string(s))

       def array = surround("[", "]")(
         value sep "," map (vs => JArray(vs.toIndexedSeq))
       ) scope "array"

       def obj = surround("{", "}")(
         keyval sep "," map (kvs => JObject(kvs.toMap))
       ) scope "object"

       def keyval = escapedQuoted ** (":" *> value)

       def lit = scope("literal") {
         "null".as(JNull) |
           double.map(JNumber(_)) |
           escapedQuoted.map(JString(_)) |
           "true".as(JBool(true)) |
           "false".as(JBool(false))
       }

       def value: Parser[JSON] = lit | obj | array

       root(whitespace *> (obj | array))
     }
   }*/

}
