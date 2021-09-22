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
  // trick:
  sealed trait DB[A]
  case class Get[A](key: String, callback: Int => DB[A]) extends DB[A]
  // Unit only has one single member ()
  case class Put[A](key: String, value: Int, callback: Unit => DB[A]) extends DB[A]
  case class Return[A](result: A) extends DB[A]

  val p1: DB[Int] = Put("Mike", 15, (_) =>
                    Get("Mike", x =>
                    Put("Mike", x + 1, (_) =>
                    Get("Mike", y =>
                    Return(x+y)))))

  def runDB[A](program: DB[A], db: Map[String, Int]): A =
    program match {
      case Get(key, callback) => 
        runDB(callback(db(key)), db)
      case Put(key, value, callback) =>
        callback   db + (key -> value)
      case Return(result) => result
    }
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
