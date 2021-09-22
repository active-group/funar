package funar

import cats._
import cats.implicits._ 
import cats.data._

object Free {
/*
  sealed trait Reader[Env, A]
  case class Ask[Env, A](callback: Env => Reader[Env, A]) extends Reader[Env, A]
  case class ReturnR[Env, A](result: A) extends Reader[Env, A]
*/

  sealed trait ReaderF[Env, SelfReference]
  case class Ask[Env, SelfReference](callback: Env => SelfReference) extends ReaderF[Env, SelfReference]

  sealed trait DBF[SelfReference]
  case class Get[SelfReference](key: String, callback: Int => SelfReference)
    extends DBF[SelfReference]
  case class Put[SelfReference](key: String, value: Int, callback: Unit => SelfReference) 
    extends DBF[SelfReference]

  // F is something like DBF, ReaderF[Env, _]
  sealed trait Free[F[_], A] {
    // this would work if we could say that F is a functor
    /*
    def flatMap[B](next: A => Free[F, B]): Free[F, B] =
      this match {
        case Pure(result) => next(result)
        case Impure(command) => ???
//          Impure(command)
      }
    */
  }
  case class Pure[F[_], A](result: A) extends Free[F, A]
  case class Impure[F[_], A](command: F[Free[F, A]]) extends Free[F, A]

  implicit def freeMonad[F[_]](implicit fFunctor: Functor[F]): Monad[Free[F, *]] = new Monad[Free[F, *]] { // * says this is the type parameter for the monad
    def pure[A](x: A): Free[F,A] = Pure(x)
    def flatMap[A, B](fa: Free[F,A])(next: A => Free[F,B]): Free[F,B] =
      fa match {
        case Pure(result) => next(result)
        case Impure(command) =>
          Impure(command.map(free => free.flatMap(next)))
      }
    def tailRecM[A, B](a: A)(f: A => Free[F,Either[A,B]]): Free[F,B] = ???
  } 



}