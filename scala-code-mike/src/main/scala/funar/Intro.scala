package funar

object Intro {

  sealed trait Liveness // trait - think "interface"
  case object Alive extends Liveness // case: automatically define constructors, equality, etc.
  case object Dead extends Liveness
}
