package funar

object Intro {

  sealed trait Liveness
  case object Alive extends Liveness
  case object Dead extends Liveness
}
