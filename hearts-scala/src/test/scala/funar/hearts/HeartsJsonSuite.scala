package funar.hearts

import io.circe.parser.parse
import io.circe.ParsingFailure
import cats._
import cats.implicits._
import cats.data._

class HeartsJsonSuite extends munit.FunSuite {
  import HeartsJson._
  import funar.json.Decode._
  import GameEvent._
  import GameCommand._

  def decode[A](decoder: Decoder[A], s: String): Either[Either[ParsingFailure, Error], A] =
    parse(s) match {
      case Right(v) => 
        decoder(v).swap.map(Right(_)).swap
      case Left(f) => Left(Left(f))
    }

  test("mapDecoder") {
    val d1 = mapDecoder[Int, Int](int, int)
    assertEquals(decode(d1, "[[1, 2],[3,4]]"), Right(Map(1 -> 2, 3 -> 4)))
    val s = """[
          [
            {
              "playerName" : "bonzo",
              "playerId" : "1"
            },
            [
              {
                "suit" : "Diamonds",
                "rank" : "Eight"
              },
              {
                "suit" : "Clubs",
                "rank" : "Eight"
              }
            ]
          ]
        ]"""
    val d2 = mapDecoder[Player, List[Card]](playerDecoder, list(cardDecoder))
    assertEquals(decode(d2, s), 
                 Right(Map(Player(name="bonzo", id="1") -> 
                        List(Card(Suit.Diamonds, Rank.Eight),
                             Card(Suit.Clubs, Rank.Eight)))))
  }

  test("dataDecoder") {
    val d = dataDecoder[String](Map("i" -> int.map(_.toString()), "s" -> string), tag => fail("unknown tag " + tag))
    assertEquals(decode(d, """{ "tag" : "s", "contents" : "foo" }"""), Right("foo"))
  }

  test("constructorDecoder1Nested") {
    val d = constructorDecoder1Nested[Int, Some[Int]](int, Some(_))
    assertEquals(decode(d, """[1]"""), Right(Some(1)))
  }

  test("constructorDecoder1") {
    val d = constructorDecoder1(mapDecoder(int, int), (x: Map[Int, Int]) => x)
    assertEquals(decode(d, """[[1, 2], [3, 4]]"""), Right(Map(1 -> 2, 3 -> 4)))
  }

  test("dealHandsDecoder") {
    val contents = """[
          [
            {
              "playerName" : "bonzo",
              "playerId" : "1"
            },
            [
              {
                "suit" : "Diamonds",
                "rank" : "Eight"
              },
              {
                "suit" : "Clubs",
                "rank" : "Eight"
              }
            ]
          ]
        ]""" 
    assertEquals(decode(dealHandsDecoder, contents), 
      Right(DealHands(Map(Player(name="bonzo", id="1") -> 
                        Set(Card(Suit.Diamonds, Rank.Eight),
                            Card(Suit.Clubs, Rank.Eight))))))
  }

  test("DealHands") {
      val dealHands1 =
        """{
          "tag" : "DealHands",
          "contents" : [
            [
              {
                "playerName" : "bonzo",
                "playerId" : "1"
              },
              [
                {
                  "suit" : "Diamonds",
                  "rank" : "Eight"
                },
                {
                  "suit" : "Clubs",
                  "rank" : "Eight"
                }
              ]
            ],
            [
              {
                "playerName" : "bonzo",
                "playerId" : "2"
              },
              [
                {
                  "suit" : "Hearts",
                  "rank" : "Ten"
                },
                {
                  "suit" : "Clubs",
                  "rank" : "Ace"
                }
              ]
            ],
            [
              {
                "playerName" : "bonzo",
                "playerId" : "3"
              },
              [
                {
                  "suit" : "Spades",
                  "rank" : "Seven"
                },
                {
                  "suit" : "Clubs",
                  "rank" : "Three"
                }
              ]
            ],
            [
              {
                "playerName" : "bonzo",
                "playerId" : "4"
              },
              [
                {
                  "suit" : "Spades",
                  "rank" : "Queen"
                },
                {
                  "suit" : "Hearts",
                  "rank" : "Five"
                }
              ]
            ]
          ]
        }"""
      val dealHands =
        """{
          "tag" : "DealHands",
          "contents" : [
            [
              {
                "playerName" : "bonzo",
                "playerId" : "1"
              },
              [
                {
                  "suit" : "Diamonds",
                  "rank" : "Eight"
                },
                {
                  "suit" : "Clubs",
                  "rank" : "Eight"
                }
              ]
            ]
          ]
        }"""
      assertEquals(decode(gameCommandDecoder, dealHands),
        Right(DealHands(Map(
        Player(
          id = "1",
          name = "bonzo"
        ) -> Set(
          Card(
            suit = Suit.Diamonds,
            rank = Rank.Eight
          ),
          Card(
            suit = Suit.Clubs,
            rank = Rank.Eight
          ))))))
    }
}

