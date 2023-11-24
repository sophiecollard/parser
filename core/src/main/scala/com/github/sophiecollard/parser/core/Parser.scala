package com.github.sophiecollard.parser.core

import cats.implicits._

import scala.math.pow
import scala.util.Try

trait Parser[A] {

  def parse(input: String): Parser.Result[A]

  final def map[B](f: A => B): Parser[B] =
    Parser.instance(input => parse(input).map { case (a, rem) => (f(a), rem) })

  final def map2[B, C](other: => Parser[B])(f: (A, B) => C): Parser[C] =
    Parser.instance { input =>
      parse(input).flatMap { case (a, rem1) =>
        other.parse(rem1).map { case (b, rem2) =>
          (f(a, b), rem2)
        }
      }
    }

  final def as[B](b: B): Parser[B] = map(_ => b)

  final def |(other: => Parser[A]): Parser[A] =
    Parser.instance(input => parse(input) orElse other.parse(input))

  final def ~[B](other: => Parser[B]): Parser[(A, B)] =
    Parser.instance { input =>
      parse(input).flatMap { case (a, rem1) =>
        other.parse(rem1).map { case (b, rem2) =>
          ((a, b), rem2)
        }
      }
    }

  final def *>[B](other: => Parser[B]): Parser[B] = (this ~ other).map(_._2)

  final def <*[B](other: => Parser[B]): Parser[A] = (this ~ other).map(_._1)

  final def opt: Parser[Option[A]] = map(Option(_)) | Parser.pure(None)

  final def either[B](other: => Parser[B]): Parser[Either[A, B]] = map(_.asLeft[B]) | other.map(_.asRight[A])

  final def rep: Parser[List[A]] = map2(rep0)(_ :: _)

  final def rep0: Parser[List[A]] = rep | Parser.pure(Nil)

  final def sep(sepP: Parser[Char]): Parser[List[A]] =
    (this ~ (sepP *> this).rep0).map(_ :: _) | Parser.pure(Nil)

}

object Parser {

  def apply[A](implicit ev: Parser[A]): Parser[A] = ev

  def instance[A](f: String => Result[A]): Parser[A] =
    new Parser[A]:
      override def parse(input: String): Result[A] =
        f(input)

  def pure[A](a: A): Parser[A] = instance(input => Right((a, input)))

  def char(c: Char): Parser[Char] =
    instance { input =>
      for {
        headChar <- input.headOption.toRight(Failure(s"Failed to parse char '$c' from empty input"))
        result <- if (headChar == c)
          (headChar, input.drop(1)).asRight[Failure]
        else
          Failure(s"Failed to parse char '$c' from $input").asLeft[Success[Char]]
      } yield result
    }

  def chompWhile(f: Char => Boolean): Parser[String] =
    instance { input =>
      val output = input.toCharArray.takeWhile(f).mkString
      Right((output, input.drop(output.length)))
    }

  def string(value: String): Parser[String] =
    def combine(tp: Parser[String], hp: Parser[Char]): Parser[String] = tp.map2(hp)(_ + _)
    value.map(char).foldLeft(pure(""))(combine)

  val boolean: Parser[Boolean] =
    val trueP = string("true").as(true)
    val falseP = string("false").as(false)
    trueP | falseP

  val digit: Parser[Int] =
    instance { input =>
      for {
        headChar <- input.headOption.toRight(Failure(s"Failed to parse digit from empty input"))
        result <- Try(headChar.toString.toInt).toEither.map(n => (n, input.drop(1))).leftMap(e => Failure(e.getMessage))
      } yield result
    }

  val uInt: Parser[Int] = digit.rep.map(_.reverse.zipWithIndex.map { case (d, i) => d * pow(10, i).toInt }.sum)

  val int: Parser[Int] =
    val minusSignP = char('-').opt
    (minusSignP ~ uInt).map {
      case (Some(_), n) => n * -1
      case (None, n)    => n
    }

  final case class Failure(message: String)
  type Success[A] = (A, String)
  type Result[A] = Either[Failure, Success[A]]

}
