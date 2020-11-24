# Vorbereitung auf den Vorkurs

# Technische Vorbereitung

Bei der Schulung ist es sinnvoll, den Code nachzuvollziehen
bzw. eigenen zu schreiben.

## Vorkurs

Für den Vorkurs bitte Racket installieren, mindestens Version 7.8:

https://download.racket-lang.org/

## Hauptteil

Für die Vorbereitung gibt es zwei Optionen:

### Falls Sie schon Haskell-erfahren sind und Cabal installiert ist:

```
cd haskell-code
cabal install --dependencies-only
```

### Falls nicht:

- [Docker](https://www.docker.com/) installieren
- der Docker-VM ggf. mindestens 6DB Speicher geben
- im Verzeichnis `docker-ghcide` den Befehl `docker build -t ghcide .`
  absetzen (dauert eine Weile)
- [Visual Studio Code](https://code.visualstudio.com/download) installieren
- die Extension "Remote - Containers" installieren:
  Auf das Extensions-Icon links klicken, nach "Containers" suchen,
  "Remote - Containers" anwählen, auf "Install" klicken
- auf das Datei-Icon links oben klicken
- oben im Menü "View" -> "Command Palette", dort
  "containers" tippen, "Remote - Containers: Open Folder in Container" selektieren
- das Verzeichnis `haskell-code` selektieren

Da sollte jetzt eine Meldung erscheinen, dass ein Docker-Image gebaut
wird.  Das kann eine Weile dauern, sollte aber ohne Fehlermeldung
vonstatten gehen.

# Bei Problemen:

... bitte Mike Sperber kontaktieren.

## Elm

Falls Model-View-Update drankommt, werden wir Elm brauchen.  Anleitung
hier:

https://guide.elm-lang.org/install/
