package com.github.sophiecollard.parser.core

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserSpec extends AnyWordSpec with Matchers {
  "The map method" should {
    "transform a Parser[A] instance into a Parser[B] instance" in {
      val p = Parser.int.map(n => s"Got number $n")
      p.parse("123") shouldBe Right(("Got number 123"), "")
    }
  }
}
