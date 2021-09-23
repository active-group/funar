package funar.hearts

object Servers {

  case class Mike()
  val mike = Player("1", "Mike")
  case class Peter()
  val peter = Player("2", "Peter")
  case class Nicole()
  val nicole = Player("3", "Nicole")
  case class Annette()
  val annette = Player("4", "Annette")

  def startServers() = {
    TableServer.fiber(8080, List(mike, peter, nicole, annette))
    PlayerServer.alongFiber[Mike](8001, mike)
    PlayerServer.alongFiber[Peter](8002, peter)
    PlayerServer.alongFiber[Nicole](8003, nicole)
    PlayerServer.alongFiber[Annette](8004, annette)
  }
}