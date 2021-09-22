package funar

// listMap[A, B](f: A => B, list: List[A]): List[B]

object Functor {
  def optionMap[A, B](f: A => B, option: Option[A]): Option[B] =
    option match {
      case None => None
      case Some(x) => Some(f(x))
    }
}
