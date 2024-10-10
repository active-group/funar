package funar.hearts

class CardsSuite extends munit.FunSuite {
  test("rank ordering") {
    import Rank._
    val ranks: Seq[Rank] = Seq(Jack, Three, Queen, Six, Ten, Four, Nine, King, Two,
                               Seven, Ace, Eight, Five)
    assertEquals(ranks.sorted, Rank.all)
  } 
}