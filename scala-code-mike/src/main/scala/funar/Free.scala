package funar

object Free {
/*
  sealed trait Reader[Env, A]
  case class Ask[Env, A](callback: Env => Reader[Env, A]) extends Reader[Env, A]
  case class ReturnR[Env, A](result: A) extends Reader[Env, A]
*/

  sealed trait ReaderF[Env, SelfReference]
  case class Ask[Env, SelfReference](callback: Env => SelfReference) extends ReaderF[Env, SelfReference]

  


}