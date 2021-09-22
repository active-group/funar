package funar

/*
DSL for database access / dependency injection
key-value store: String |-> Int

put("Mike", 15)
x = get("Mike")
put("Mike", x+1)
y = get("Mike")
return (x + y)
*/

object DB {
/* FAIL:
  sealed trait DBCommand
  case class Put(key: String, value: Int) extends DBCommand
  case class Get(key: String) extends DBCommand
  case class Return(result: Int) extends DBCommand

  type DBProgram = List[DBCommand]

  val p1 = List(Put("Mike", 15), Get("Mike"), Put("Mike", ???))
*/
  sealed trait DB
  case class Get(key: String)

}

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
