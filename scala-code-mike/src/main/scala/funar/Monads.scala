package funar

trait Functor[F[_]] { // [_]: F is a type constructor
  def map[A, B](f: A => B, x: F[A]): F[B]
}

// listMap[A, B](f: A => B, list: List[A]): List[B]

object Functor {
  def optionMap[A, B](f: A => B, option: Option[A]): Option[B] =
    option match {
      case None => None
      case Some(x) => Some(f(x))
    }

  def listFunctor = new Functor[List] {
    def map[A, B](f: A => B, x: List[A]): List[B] =
      x.map(f)
  }


}
