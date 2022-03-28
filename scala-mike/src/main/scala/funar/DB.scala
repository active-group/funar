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
enum DB[A] {
  case Get(key: String, callback: Int => DB[A])
  case Put(key: String, value: Int, callback: Unit => DB[A])
  case Return(result: A)
}

import DB._

val p1 = Put("Mike", 50, {() =>
         Get("Mike", {x =>
         Put("Mike", x+1, {() =>
         Get("Mike", {y =>
         Return(x+y)})})})  })