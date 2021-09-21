package funar

object Intro {

  // Scala 2 enumerations: design mistake

  sealed trait Liveness // trait - think "interface"
//  case object Alive extends Liveness // case: automatically define constructors, equality, etc.
//  case object Dead extends Liveness

  // companion object
  object Liveness {
    case object Alive extends Liveness
    case object Dead extends Liveness
  }

  type Weight = Int

  case class Dillo(liveness: Liveness, weight: Weight) {
    def runOver: Dillo = // no arguments: empty parens
      this.copy(liveness = Liveness.Dead)
  }

  // values
  val dillo1 = Dillo(Liveness.Alive, 10)
  val dillo2 = Dillo(Liveness.Dead, 11)
  
  // function
  def runOverDillo(dillo: Dillo): Dillo =
    // Dillo(Liveness.Dead, dillo.weight)
    // dillo.copy(liveness = Liveness.Dead)
    dillo match {
      case Dillo(Liveness.Dead, _) => dillo
      case Dillo(_, w) => Dillo(Liveness.Dead, w)
    }

  case class Snake(length: Int, thickness: Int)

  val snake1 = Snake(200, 5)
  val snake2 = Snake(300, 10)

  def runOverSnake(snake: Snake): Snake =
    snake.copy(thickness = 0)

}
