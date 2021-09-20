# Technische Vorbereitung

Bei der Schulung ist es sinnvoll, den Code nachzuvollziehen
bzw. eigenen zu schreiben.

## Vorkurs

Für den Vorkurs bitte Racket installieren, mindestens Version 8.0:

https://download.racket-lang.org/

## Scala

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
sbt new scala/hello-world.g8
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

## Haskell

- [Docker](https://www.docker.com/) installieren
- der Docker-VM ggf. mindestens 6GB Speicher geben
- [Visual Studio Code](https://code.visualstudio.com/download) installieren
- die Extension "Remote - Containers" installieren:
  Auf das Extensions-Icon links klicken, nach "Containers" suchen,
  "Remote - Containers" anwählen, auf "Install" klicken
- auf das Datei-Icon links oben klicken
- oben im Menü "View" -> "Command Palette", dort
  "containers" tippen, "Remote - Containers: Open Folder in Container" selektieren
- das Verzeichnis `haskell-code` selektieren
- dort die Datei `Intro.hs` aufmachen und etwas ändern

Da sollte jetzt eine Meldung erscheinen, dass ein Docker-Image gebaut
wird.  Das kann eine Weile dauern, sollte aber ohne Fehlermeldung
vonstatten gehen.

# Bei Problemen:

... bitte Mike Sperber kontaktieren.

## Elm

Falls Model-View-Update drankommt, werden wir Elm brauchen.  Anleitung
hier:

https://guide.elm-lang.org/install/
