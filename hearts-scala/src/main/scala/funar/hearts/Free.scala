package funar.hearts

import cats._
import cats.implicits._
import cats.data._
import scala.annotation.tailrec

enum Free[F[_], A] {
  case Pure[F[_], A](result: A) extends Free[F[_], A]
  case Impure[F[_], A](f: F[Free[F, A]]) extends Free[F[_], A]
}

object Free {
  implicit def freeMonad[F[_]](implicit fFunctor: Functor[F]): Monad[Free[F, _]] =
    new Monad[Free[F, _]] {
      def pure[A](value: A) = Pure[F, A](value)

      def flatMap[A, B](fa: Free[F, A])(k: A => Free[F, B]): Free[F, B] =
        fa match {
          case Pure(result) => k(result)
          case Impure(f) => Impure(fFunctor.map(f)(_.flatMap(k)))
        }

      def tailRecM[A, B](a: A)(next: A => Free[F, Either[A, B]]): Free[F, B] = {
        def loop(a: A): Free[F, B] =
          next(a).flatMap {
            case Left(a0) => tailRecM(a0)(next)
            case Right(b) => Pure(b)
          }

        loop(a)
      }
    }

}