# Schulung "Funktionale Softwarearchitektur" mit Scala

## Vorkurs

Für den Vorkurs bitte Racket installieren, mindestens Version 8.0:

https://download.racket-lang.org/

## Technische Vorbereitung

sbt installieren:

https://www.scala-sbt.org/

Visual Studio Code installieren:

https://code.visualstudio.com/

Das VSCode-Binary `code` sollte im Pfad sein.  Auf dem Mac muß man
dafür folgendes machen:

https://code.visualstudio.com/docs/setup/mac#_command-line

In VSCode die Extension "Metals" installieren.

Beispielprojekt erzeugen, als Name `scala-example` wählen:

```
sbt new scala/scala3.g8
```

Dann:

```
cd scala-example
```

Dann den Order `scala-example` in VSCode öffnen.  Die Metals-Extension
sollte sich melden. 

Wenn die alles heruntergeladen hat, als Test die Datei `Main.scala`
öffnen und dort absichtlich Fehler machen - die sollten dann rot
unterkringelt sein.

# Bei Problemen:

... bitte Mike Sperber kontaktieren.

## Elm

Falls Model-View-Update drankommt, werden wir Elm brauchen.  Anleitung
hier:

https://guide.elm-lang.org/install/

## Harts starten

Im `hearts`-Verzeichnis:

```
sbt console
funar.hearts.Servers.startServers()
```

Dann im `hearts-frontend`-Verzeichnis:

```
elm reactor
```

Dann im Browser auf
[`http://localhost:8000/src/HeartsFrontend.elm`](http://localhost:8000/src/HeartsFrontend.elm) gehen.


