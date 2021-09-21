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

  case class Dillo(liveness: Liveness, weight: Weight)

  // values
  val dillo1 = Dillo(Liveness.Alive, 10)
  val dillo2 = Dillo(Liveness.Dead, 11)

  // function
  def runOverDillo(dillo: Dillo): Dillo =
    Dillo(Liveness.Dead, dillo.weight)


}
