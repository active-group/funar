package funar

sealed trait Suit
object Suit {
  case object Diamonds extends Suit
  case object Clubs extends Suit
  case object Spades extends Suit
  case object Hearts extends Suit

  val all = Seq(Diamonds, Clubs, Spades, Hearts)
}

sealed trait Rank {
  val value: Int
}

object Rank {
  case object Two extends Rank { override val value = 2 }
  case object Three extends Rank { override val value = 3 }
  case object Four extends Rank { override val value = 4 }
  case object Five extends Rank { override val value = 5 }
  case object Six extends Rank { override val value = 6 }
  case object Seven extends Rank { override val value = 7 }
  case object Eight extends Rank { override val value = 8 }
  case object Nine extends Rank { override val value = 9 }
  case object Ten extends Rank { override val value = 10 }
  case object Jack extends Rank { override val value = 11 }
  case object Queen extends Rank { override val value = 12 }
  case object King extends Rank { override val value = 13 }
  case object Ace extends Rank { override val value = 14 }

  val all = Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
                           Jack, Queen, King, Ace)
}



