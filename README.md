# Funktionale Softwarearchitektur, 17.-20.10.2022

# Technische Vorbereitung

Bei der Schulung ist es sinnvoll, den Code nachzuvollziehen
bzw. eigenen zu schreiben.

## Vorkurs

Für den Vorkurs bitte Racket installieren, mindestens Version 8.0:

https://download.racket-lang.org/

## Hauptteil

- [Docker](https://www.docker.com/) installieren
- für macOS und Windows: der Docker-VM ggf. mindestens 6GB Speicher
  geben
- [Visual Studio Code](https://code.visualstudio.com/download) installieren
- die Extension "Dev Containers" installieren:
  Auf das Extensions-Icon links klicken, nach "Containers" suchen,
  "Dev Containers" anwählen, auf "Install" klicken
- auf das Datei-Icon links oben klicken
- oben im Menü "View" -> "Command Palette", dort
  "containers" tippen, "Remote - Containers: Open Folder in Container"
  selektieren
- das Verzeichnis `haskell-code` selektieren

Da sollte jetzt eine Meldung erscheinen, dass ein Docker-Image gebaut
wird.  Das kann eine Weile dauern, sollte aber ohne Fehlermeldung
vonstatten gehen.

- in den Extension-Settings für `haskell.haskell` die Option "Haskell:
  Manage HLS" auf `PATH` setzen
- wenn das `direnv`-Plugin nachfragt, auf `Allow` klicken, und
  evtl. danach den Reload akzeptieren
- die Datei `Intro.hs` aufmachen und etwas ändern

Nach etwas Ladezeit (die erforderlichen Haskell-Pakete müssen
heruntergeladen werden) sollten in der Datei Anmerkungen des Linters
erscheinen.

# Bei Problemen:

... bitte Mike Sperber kontaktieren.

## Elm

Falls Model-View-Update drankommt, werden wir Elm brauchen.  Anleitung
hier:

https://guide.elm-lang.org/install/
