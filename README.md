# Technische Vorbereitung

Bei der Schulung ist es sinnvoll, den Code nachzuvollziehen bzw. eigenen zu
schreiben.

## Vorkurs

Für den Vorkurs bitte Racket installieren, mindestens Version 8.7:

https://download.racket-lang.org/

## Hauptteil

Je nach Betriebssystem haben sich unterschiedliche Installationsmethoden als
universell brauchbar herauskristallisiert. Die Windows-Variante baut auf Docker
auf und sollte grundsätzlich auch auf Linux funktionieren, hat aber in der
Vergangenheit in speziellen Setups Probleme durch UID-Mapping bereitet.

### Via Docker (Windows, macOS, Linux, ...)

- [Docker](https://www.docker.com/) installieren
- für macOS und Windows: der Docker-VM ggf. mindestens 6GB Speicher geben
- [Visual Studio Code](https://code.visualstudio.com/download) installieren
- die Extension "Dev Containers" (`ms-vscode-remote.remote-containers`)
  installieren: Auf das Extensions-Icon links klicken, nach "Containers" suchen,
  "Dev Containers" anwählen, auf "Install" klicken
- auf das Datei-Icon links oben klicken
- oben im Menü "View" -> "Command Palette", dort "containers" tippen, "Remote -
  Containers: Open Folder in Container" selektieren
- das Top-Level-Verzeichnis (Ort des geklonten Repos) selektieren

Da sollte jetzt eine Meldung erscheinen, dass ein Docker-Image gebaut bzw.
geladen wird. Das kann eine Weile dauern, sollte aber ohne Fehlermeldung
vonstatten gehen.

- In der Datei `haskell-code/Intro.hs` das `module` verunstalten (z. B. einen
  Buchstaben entfernen)

Nach etwas initialer Ladezeit sollten in der Datei Anmerkungen des Linters
erscheinen.

#### UID-Remapping-Problem (u. U. nur Linux)

Falls im Docker-Container Berechtigungsprobleme auftreten, kann es helfen, das
Image bauen zu lassen (statt herunterzuladen). Dazu:

- In [devcontainer.json](./.devcontainer/devcontainer.json) statt `"image"` die
  Zeile mit `"dockerFile"` einkommentieren
- Im [Dockerfile](./.devcontainer/Dockerfile) im dokumentierten Abschnitt die
  `1000` durch die jeweils eigene UID und GID ersetzen
- VSCode neustarten und den Ordner wieder im Container öffnen -> es sollte nun
  ein Image frisch gebaut werden, was etwas dauert

### Via Nix-Installation (Linux, macOS, vmtl. WSL2)

- Die Single-User-Variante von [Nix
  installieren](https://nixos.org/download.html#nix-install-linux) (Multi-User
  funktioniert auch, ist aber schwieriger wieder vom System zu entfernen, falls
  man das möchte)
- [Flakes einschalten](https://nixos.wiki/wiki/Flakes): in der Regel bedeutet
  das, in `~/.config/nix/nix.conf` die folgende Zeile einzutragen:

  ```
  experimental-features = nix-command flakes
  ```

- `cd <path-to>/funar`
- `nix develop .#withVSCode` ausführen -> Sie befinden sich nun in einer
  projektspezifischen, entsprechend gekennzeichneten Umgebung
- Mit `code .` VSCode in diesem Ordner öffnen
- In der Datei `haskell-code/Intro.hs` das `module` verunstalten (z. B. einen
  Buchstaben beim `where` entfernen)
- Nach etwas Wartezeit (in der unteren Leiste sollte "Processing" oder ähnlich
  zu sehen sein) sollte etwas rot unterschlängelt werden

#### Anmerkungen

- Falls Sie Ihr "eigenes" VSCode nutzen möchten (auf eigene Gefahr), schreiben
  Sie bitte einfach `nix develop` (lassen also das `.#withVSCode` weg). Sie
  benötigen dann noch die Extension `haskell.haskell`.
- Diese Instruktionen funktionieren auch auf macOS, allerdings ist dort keine
  Single-User-Installation von Nix (mehr) möglich.

### Fallback: Gitpod

Falls lokale Installationen (z. B. Docker) nicht funktionieren oder nicht
erlaubt sind, ist Gitpod eine mögliche Alternative. Bei Klick auf den Button
öffnet sich eine browserbasierte VSCode-Umgebung, in der FUNAR lauffähig sein
sollte. Falls ein Popup erscheint, das fragt, wie der Haskell-Language-Server
gefunden werden soll, dann bitte "Manually via PATH" auswählen!

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/active-group/funar)

# Bei Problemen

... bitte Mike Sperber kontaktieren.

## Elm

Falls Model-View-Update drankommt, werden wir Elm brauchen. Anleitung zur
Installation hier:

https://guide.elm-lang.org/install/

Falls Sie `nix` verwenden, dann ist `elm` bereits in der Entwicklungsumgebung
(`nix develop`) vorhanden.

# Literatur

- [Blog Funktionale Programmierung](https://funktionale-programmierung.de/)
- Michael Sperber, Herbert Klaeren: [Schreibe Dein Programm!](https://www.deinprogramm.de/sdp/)
- Sandy Maguire: [Algebra-Driven Design](https://leanpub.com/algebra-driven-design)
- Michael Pilquist, Rúnar Bjarnason, and Paul Chiusano [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala-second-edition)
- Scott Wlaschin: [Domain Modeling Made Functional](https://pragprog.com/titles/swdddf/domain-modeling-made-functional/)
- Alexis King: [Parse, Don't Validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
- Markus Schlegel: [Denotational Design](https://funktionale-programmierung.de/2024/02/27/denotational-design-01.html)
- Scott Wlaschin: [Moving IO to the edges of your app: Functional Core, Imperative Shell](https://www.youtube.com/watch?v=P1vES9AgfC4)
- [Copilot](https://copilot-language.github.io/) (DSL für Hard-Real-Time-Code, mit C-Code-Generierung)
- [Arrows](https://www.haskell.org/arrows/)
- [Monaden in Kotlin](https://funktionale-programmierung.de/2023/05/22/kotlin-monads.html), [Vortrag](https://www.youtube.com/watch?v=yaPuxQACdn8qI)
- [Vavr](https://vavr.io/) (funktionale Programmierung in Java/Kotlin):
- [Wie Effekte strukturieren](https://www.youtube.com/watch?v=Yx8z9M7XHu0)
- [Financial Contracts](https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/)


<!-- Local Variables: -->
<!-- fill-column: 80 -->
<!-- End: -->
