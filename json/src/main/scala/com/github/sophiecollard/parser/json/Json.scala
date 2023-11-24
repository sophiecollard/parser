package com.github.sophiecollard.parser.json

import com.github.sophiecollard.parser.core.Parser

sealed trait Json {

  final def noSpaces: String =
    this match
      case Json.Null           => "null"
      case Json.Boolean(value) => s"$value"
      case Json.Int(value)     => s"$value"
      case Json.Double(value)  => s"$value"
      case Json.String(value)  => s""""$value""""
      case Json.Array(value)   => s"[${value.map(_.noSpaces).mkString(",")}]"
      case Json.Object(value)  => s"{${value.map { case (k, v) => s""""$k": ${v.noSpaces}""" }.mkString(",")}}"

  final def asNull: Option[Json.Null.type] =
    this match
      case Json.Null => Some(Json.Null)
      case _         => None

  final def asBoolean: Option[Json.Boolean] =
    this match
      case b: Json.Boolean => Some(b)
      case _               => None

  final def asInt: Option[Json.Int] =
    this match
      case i: Json.Int => Some(i)
      case _           => None

  final def asDouble: Option[Json.Double] =
    this match
      case d: Json.Double => Some(d)
      case _              => None

  final def asString: Option[Json.String] =
    this match
      case s: Json.String => Some(s)
      case _              => None

  final def asArray: Option[Json.Array] =
    this match
      case a: Json.Array => Some(a)
      case _             => None

  final def asObject: Option[Json.Object] =
    this match
      case o: Json.Object => Some(o)
      case _              => None

}

object Json {

  case object Null extends Json
  final case class Boolean(value: scala.Boolean) extends Json
  sealed trait Number extends Json
  final case class Int(value: scala.Int) extends Number
  final case class Double(value: scala.Double) extends Number
  final case class String(value: java.lang.String) extends Json
  final case class Array(value: List[Json]) extends Json
  final case class Object(value: Map[java.lang.String, Json]) extends Json

  val `null`: Json = Null
  def apply(value: scala.Boolean): Json = Boolean(value)
  def apply(value: scala.Int): Json = Int(value)
  def apply(value: scala.Double): Json = Double(value)
  def apply(value: java.lang.String): Json = String(value)
  def apply(value: List[Json]): Json = Array(value)
  def apply(value: Map[java.lang.String, Json]): Json = Object(value)

  implicit val parser: Parser[Json] =
    val nullP: Parser[Json] = Parser.string("null").as(Null)
    val booleanP: Parser[Json] = Parser.boolean.map(Boolean(_))
    val intP: Parser[Json] = Parser.int.map(Int(_))
    val doubleP: Parser[Json] = (Parser.int ~ (Parser.char('.') *> Parser.uInt)).map {
      case (beforeDecimal, afterDecimal) =>
        Json(s"$beforeDecimal.$afterDecimal".toDouble) // TODO Make sure this is safe
    }
    val stringP: Parser[Json] = (Parser.char('"') *> Parser.chompWhile(_ != '"') <* Parser.char('"')).map(String(_))
    lazy val arrayP: Parser[Json] =
      val (startP, endP, sepP) = (Parser.char('['), Parser.char(']'), Parser.char(','))
      (startP *> jsonP.sep(sepP) <* endP).map(Array(_))
    lazy val objectP: Parser[Json] =
      val (startP, endP, sepP) = (Parser.char('{'), Parser.char('}'), Parser.char(','))
      val keyP: Parser[java.lang.String] = Parser.char('"') *> Parser.chompWhile(_ != '"') <* Parser.char('"')
      val keyValueP: Parser[(java.lang.String, Json)] = keyP ~ (Parser.char(':') ~ Parser.char(' ').rep0 *> jsonP)
      (startP *> keyValueP.sep(sepP) <* endP).map(kvs => Object(kvs.toMap))
    lazy val jsonP = objectP | arrayP | stringP | doubleP | intP | booleanP | nullP
    jsonP

}
