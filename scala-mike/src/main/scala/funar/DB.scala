package funar

// Datenbank-Zugriff funktional

// put("Mike", 50)
// x = get("Mike")
// put("Mike", x+1)
// y = get("Mike")
// return (x + y)

// 1. Idee: weg von "Funktion macht was"
// hin zu: Funktion generiert eine Beschreibung von dem,
// was sie machen will

enum DBCommand[A] {
  case Put(key: String, value: Int)
  case Get(key: String)
  case Return(result: A)
}
