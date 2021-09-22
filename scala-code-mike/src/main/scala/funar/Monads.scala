package funar

trait Functor[F[_]] {
  def map[A, B](f: A => B, x: F[A]): F[B]
}

// listMap[A, B](f: A => B, list: List[A]): List[B]

object Functor {
  def optionMap[A, B](f: A => B, option: Option[A]): Option[B] =
    option match {
      case None => None
      case Some(x) => Some(f(x))
    }
}
