package funar

// Datenbank-Zugriff funktional

// put("Mike", 50)
// x = get("Mike")
// put("Mike", x+1)
// y = get("Mike")
// return ((x + y).toString)

// 1. Idee: weg von "Funktion macht was"
// hin zu: Funktion generiert eine Beschreibung von dem,
// was sie machen will

/*
enum DBCommand[A] {
  case Put(key: String, value: Int)
  case Get(key: String)
  case Return(result: A)
}

type DBProgram[A] = List[DBCommand[A]]

val p1 = List(Put("Mike", 50),
              Get("Mike"),
              // hier geht's nicht mehr funktioniert
*/
// "Beschreibung eines Datenbank-Programms,
//  das ein Ergebnis vom Typ A liefert."
enum DB[A] {
  case Get(key: String,             callback: Int  => DB[A])
  case Put(key: String, value: Int, callback: Unit => DB[A])
  case Return(result: A)

  // magischer Name
  def flatMap[B](cont: A => DB[B]): DB[B] = DB.splice(this, cont)
}

import DB._

object DB {
  val p1 = Put("Mike", 50, {_ =>
          Get("Mike", {x =>
          Put("Mike", x+1, {_ =>
          Get("Mike", {y =>
          Return(x+y)})})})})

  // get, put einzeln
  def get(key: String): DB[Int] =
    Get(key, { value => Return(value)})

  def put(key: String, value: Int): DB[Unit] =
    Put(key, value, { _ => Return(())})

  // "spleißen"
  def splice[A, B](dba: DB[A], cont: A => DB[B]): DB[B] =
    dba match {
      case Get(key, callback) => 
        Get(key, { value =>
          splice(callback(value), cont) })
      case Put(key, value, callback) =>
        Put(key, value, { _ =>
          splice(callback(()), cont) })
      case Return(result) => cont(result)
    }

  val p1_a =
    splice(put("Mike", 50), { _ =>
    splice(get("Mike"), { x => 
    splice(put("Mike", x + 1), { _ =>
    splice(get("Mike"), { y => 
    Return(x+y)})})})})

  // identisch zu p1_a
  // flatMap macht DB zu einer Monade
  val p1_b =
    for {
      _ <- put("Mike", 50)
      x <- get("Mike")
      _ <- put("Mike", x+1)
      y <- get("Mike")
    } yield(x+y)
}