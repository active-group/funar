package funar.hearts

object Servers {

  val mike = Player("Mike")
  val peter = Player("Peter")
  val nicole = Player("Nicole")
  val annette = Player("Annette")

  def startServers() = {
    TableServer.fiber(8080, List(mike, peter, nicole, annette))
    PlayerServer.alongFiber(8001, mike)
    PlayerServer.alongFiber(8002, peter)
    PlayerServer.alongFiber(8003, nicole)
    PlayerServer.alongFiber(8004, annette)
  }
}