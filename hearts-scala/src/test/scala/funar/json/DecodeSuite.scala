package funar.json

import Decode._
import io.circe.{Json, ParsingFailure}
import io.circe.parser._
import cats._
import cats.implicits._

class DecodeSuite extends munit.FunSuite {

  def decode[A](decoder: Decoder[A], s: String): Either[Either[ParsingFailure, Error], A] =
    io.circe.parser.parse(s) match {
      case Right(v) => decoder(v).swap.map(Right(_)).swap
      case Left(f) => Left(Left(f))
    }

  test("string") {
    assertEquals(decode(Decode.string, "\"foo\""), Right("foo"))
  }

  test("int") {
    assertEquals(decode(Decode.int, "157"), Right(157))
  }

  test("array") {
    assertEquals(decode(Decode.list(Decode.int), "[1,2,3]"), Right(List(1,2,3)))
  }

  test("oneOf") {
    val d = oneOf(int.map(_.toString), string)
    assertEquals(decode(d, "123"), Right("123"))
    assertEquals(decode(d, "\"123\""), Right("123"))
    assertEquals(decode(d, "true"), Left(value = Right(Error.OneOf(Seq(
                                                        Error.Failure("not an int",  Json.True),
                                                        Error.Failure("not a string", Json.True))))))
  }

  test("maybe") {
    val d = optional(int)

    assertEquals(decode(d, "123"), Right(Some(123)))
    assertEquals(decode(d, "null"), Right(None))
  }

  test("index") {
    val d = index(2, int)

    assertEquals(decode(d, "[1,2,3,4]"), Right(3))
  }

  test("field") {
    val d = field("foo", int)
    assertEquals(decode(d, "{\"bar\": true, \"foo\": 23}"), Right(23))
  }

  test("map2") {
    val d = Applicative[Decoder].map2(index(0, int), index(1, int)) { (a, b) => (a+1, b+2)}
    assertEquals(decode(d, "[1,2]"), Right((2,4)))
  }

  test("monad") {
    val d1 = field("foo", int)
    val d2 = field("bar", boolean)
    val d = for {
      i <- d1
      b <- d2
    } yield (i, b)
    assertEquals(decode(d, "{\"bar\": true, \"foo\": 23}"), Right((23, true)))
  }

}
