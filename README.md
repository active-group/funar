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
bzw. geladen wird.  Das kann eine Weile dauern, sollte aber ohne Fehlermeldung
vonstatten gehen.

- In der Datei `haskell-code/Intro.hs` das `module` verunstalten (z. B. einen
  Buchstaben entfernen)

Nach etwas initialer Ladezeit sollten in der Datei Anmerkungen des Linters
erscheinen.

#### UID-Remapping-Problem (u. U. nur Linux)

Falls im Docker-Container Berechtigungsprobleme auftreten, kann
es helfen, das Image bauen zu lassen (statt herunterzuladen).  Dazu:

- In [devcontainer.json](./.devcontainer/devcontainer.json) statt
  `"image"` die Zeile mit `"dockerFile"` einkommentieren
- Im [Dockerfile](./.devcontainer/Dockerfile) im dokumentierten
  Abschnitt die `1000` durch die jeweils eigene UID und GID ersetzen
- VSCode neustarten und den Ordner wieder im Container öffnen -> es
  sollte nun ein Image frisch gebaut werden, was etwas dauert

### Via Nix-Installation (Linux, macOS, vmtl. WSL2)

- Die Single-User-Variante von [Nix
  installieren](https://nixos.org/download.html#nix-install-linux)
  (Multi-User funktioniert auch, ist aber schwieriger wieder vom
  System zu entfernen, falls man das möchte)
- `cd <path-to>/funar`
- `nix develop .#withVSCode` ausführen -> Sie befinden sich nun in
  einer projektspezifischen, entsprechend gekennzeichneten Umgebung
- Mit `code .` VSCode in diesem Ordner öffnen
- In der Datei `haskell-code/Intro.hs` das `module` verunstalten
  (z. B. einen Buchstaben beim `where` entfernen)
- Nach etwas Wartezeit (in der unteren Leiste sollte "Processing" oder
  ähnlich zu sehen sein) sollte etwas rot unterschlängelt werden

#### Anmerkungen

- Falls Sie Ihr "eigenes" VSCode nutzen möchten (auf eigene Gefahr),
  schreiben Sie bitte einfach `nix develop` (lassen also das
  `.#withVSCode` weg).  Sie benötigen dann noch die Extension
  `haskell.haskell`.
- Diese Instruktionen funktionieren auch auf macOS, allerdings ist
  dort keine Single-User-Installation von Nix (mehr) möglich.

# Bei Problemen:

... bitte Mike Sperber oder Johannes Maier kontaktieren.

## Elm

Falls Model-View-Update drankommt, werden wir Elm brauchen.  Anleitung
zur Installation hier:

https://guide.elm-lang.org/install/

Falls Sie `nix` verwenden, dann ist `elm` bereits in der
Entwicklungsumgebung (`nix develop`) vorhanden.
