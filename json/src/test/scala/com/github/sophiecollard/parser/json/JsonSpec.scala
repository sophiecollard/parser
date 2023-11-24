package com.github.sophiecollard.parser.json

import com.github.sophiecollard.parser.core.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonSpec extends AnyWordSpec with Matchers {
  "The noSpaces method" should {
    "print a null value" in {
      Json.Null.noSpaces shouldBe "null"
    }

    "print a boolean" in {
      Json(true).noSpaces shouldBe "true"
      Json(false).noSpaces shouldBe "false"
    }

    "print an int" in {
      Json(123).noSpaces shouldBe "123"
      Json(-456).noSpaces shouldBe "-456"
    }

    "print a double" in {
      Json(123.456).noSpaces shouldBe "123.456"
      Json(-789.456).noSpaces shouldBe "-789.456"
    }

    "print a string" in {
      Json("").noSpaces shouldBe """"""""
      Json("Hello, World!").noSpaces shouldBe """"Hello, World!""""
    }

    "print an array" in {
      Json(Nil).noSpaces shouldBe "[]"
      Json(List(Json("Hi!"), Json(123), Json(false))).noSpaces shouldBe """["Hi!",123,false]"""
    }

    "print an object" in {
      Json(Map.empty).noSpaces shouldBe "{}"
      Json(
        Map(
          "fst" -> Json("Hi!"),
          "snd" -> Json(123),
          "thd" -> Json(false)
        )
      ).noSpaces shouldBe """{"fst": "Hi!","snd": 123,"thd": false}"""
    }
  }

  "The Parser[Json] instance" should {
    "parse a null value" in {
      Parser[Json].parse("null") shouldBe Right((Json.Null, ""))
    }

    "parse a boolean" in {
      Parser[Json].parse("true") shouldBe Right((Json(true), ""))
      Parser[Json].parse("false") shouldBe Right((Json(false), ""))
    }

    "parse an int" in {
      Parser[Json].parse("123") shouldBe Right((Json(123), ""))
      Parser[Json].parse("-456") shouldBe Right((Json(-456), ""))
    }

    "parse a double" in {
      Parser[Json].parse("123.456") shouldBe Right((Json(123.456), ""))
      Parser[Json].parse("-456.789") shouldBe Right((Json(-456.789), ""))
    }

    "parse a string" in {
      Parser[Json].parse("""""""") shouldBe Right((Json(""), ""))
      Parser[Json].parse(""""Hello, World!"""") shouldBe Right((Json("Hello, World!"), ""))
    }

    "parse an array" in {
      Parser[Json].parse("[]") shouldBe Right((Json(Nil), ""))
      Parser[Json].parse("""["Hi!",123,false]""") shouldBe Right((Json(List(Json("Hi!"), Json(123), Json(false))), ""))
    }

    "parse an object" in {
      Parser[Json].parse("{}") shouldBe Right((Json(Map.empty), ""))
      Parser[Json].parse("""{"fst": "Hi!","snd": 123,"thd": false}""") shouldBe Right(
        (Json(Map("fst" -> Json("Hi!"), "snd" -> Json(123), "thd" -> Json(false))), "")
      )
    }
  }
}
