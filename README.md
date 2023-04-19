# Technische Vorbereitung

Bei der Schulung ist es sinnvoll, den Code nachzuvollziehen
bzw. eigenen zu schreiben.

## Vorkurs

Für den Vorkurs bitte Racket installieren, mindestens Version 8.7:

https://download.racket-lang.org/

## Hauptteil

Je nach Betriebssystem haben sich unterschiedliche
Installationsmethoden als universell brauchbar herauskristallisiert.
Die Windows-Variante baut auf Docker auf und sollte grundsätzlich auch
auf Linux funktionieren, hat aber in der Vergangenheit in speziellen
Setups Probleme durch UID-Mapping bereitet.

### Via Docker (Windows, macOS, Linux, ...)

- [Docker](https://www.docker.com/) installieren
- für macOS und Windows: der Docker-VM ggf. mindestens 6GB Speicher
  geben
- [Visual Studio Code](https://code.visualstudio.com/download) installieren
- die Extension "Dev Containers"
  (`ms-vscode-remote.remote-containers`) installieren: Auf das
  Extensions-Icon links klicken, nach "Containers" suchen, "Dev
  Containers" anwählen, auf "Install" klicken
- auf das Datei-Icon links oben klicken
- oben im Menü "View" -> "Command Palette", dort
  "containers" tippen, "Remote - Containers: Open Folder in Container"
  selektieren
- das Top-Level-Verzeichnis (Ort des geklonten Repos) selektieren

Da sollte jetzt eine Meldung erscheinen, dass ein Docker-Image gebaut
wird.  Das kann eine Weile dauern, sollte aber ohne Fehlermeldung
vonstatten gehen.

- wenn das `direnv`-Plugin nachfragt, auf `Allow` klicken, und
  evtl. danach den Reload akzeptieren
- In der Datei `haskell-code/Intro.hs` das `module` verunstalten (z. B. einen
  Buchstaben entfernen)

Nach etwas Ladezeit (die erforderlichen Haskell-Pakete müssen
heruntergeladen werden) sollten in der Datei Anmerkungen des Linters
erscheinen.

### Via Nix-Installation (Linux, macOS, vmtl. WSL2)

- Die Single-User-Variante von [Nix installieren](https://nixos.org/download.html#nix-install-linux)
- `cd <path-to>/funar`
- `nix-shell --arg withVSCode true` ausführen -> Sie befinden sich nun in einer
  projektspezifischen, entsprechend gekennzeichneten Umgebung (kann
  etwas dauern, da viel heruntergeladen werden muss)
- Mit `code .` VSCode in diesem Ordner öffnen
- In der Datei `haskell-code/Intro.hs` das `module` verunstalten (z. B. einen
  Buchstaben entfernen)
- Nach etwas Wartezeit (in der unteren Leiste sollte "Processing" oder
  ähnlich zu sehen sein) sollte etwas rot unterschlängelt werden

#### Anmerkungen
  
- Falls Sie Ihr "eigenes" VSCode nutzen möchten (auf eigene Gefahr),
  lassen Sie beim Aufruf der `nix-shell` bitte einfach das `--arg ...`
  weg.  Sie benötigen dann noch die Extension `haskell.haskell`.
- Diese Instruktionen funktionieren auch auf macOS, allerdings ist
  dort keine Single-User-Installation von Nix (mehr) möglich.  Die
  Multi-User-Variante ist etwas schwieriger wieder vom System zu
  entfernen.

# Bei Problemen:

... bitte Mike Sperber kontaktieren.

## Elm

Falls Model-View-Update drankommt, werden wir Elm brauchen.  Anleitung
hier:

https://guide.elm-lang.org/install/
