# Technische Vorbereitung

Bei der Schulung ist es sinnvoll, den Code nachzuvollziehen bzw. eigenen zu
schreiben.

## Vorkurs

Für den Vorkurs bitte Racket installieren, mindestens Version 8.7:

https://download.racket-lang.org/

## Hauptteil

Für den Hauptteil brauchen wir eine Installation von
[Haskell](https://www.haskell.org/) und eine IDE, in der Regel [Visual Studio
Code](https://code.visualstudio.com/).

Es gibt grundsätzlich zwei Installationsmethoden für Haskell, GHCup und Nix.

### GHCup

- [GHCup](https://www.haskell.org/ghcup/install/) installieren.

- Dabei alle "Default"-Optionen auswählen.
- Bei "Do you want to install haskell-language-server (HLS)?" "Yes" antworten.

Danach diese Befehle hier absetzen:

```
ghcup install ghc 9.8.4
ghcup set ghc 9.8.4
```

Um die Installation zu testen:

- Mit `code .` VSCode in diesem Ordner öffnen
- Die Extension `haskell.haskell` installieren
- In der Datei `haskell-code/Intro.hs` das `module` verunstalten (z. B. einen
  Buchstaben beim `where` entfernen)
- Nach etwas Wartezeit (in der unteren Leiste sollte "Processing" oder ähnlich
  zu sehen sein) sollte etwas rot unterschlängelt werden
, bitte VSCode starten, die Extension

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

# Bei Problemen

... bitte Mike Sperber kontaktieren.

## Elm

Falls Model-View-Update drankommt, werden wir Elm brauchen. Anleitung zur
Installation hier:

https://guide.elm-lang.org/install/

Falls Sie `nix` verwenden, dann ist `elm` bereits in der Entwicklungsumgebung
(`nix develop`) vorhanden.

# Literatur

- Michael Sperber, Herbert Klaeren: [Schreibe Dein Programm!](https://www.deinprogramm.de/sdp/)
- Sandy Maguire: [Algebra-Driven Design](https://leanpub.com/algebra-driven-design)
- Michael Pilquist, Rúnar Bjarnason, and Paul Chiusano [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala-second-edition)
- Scott Wlaschin: [Domain Modeling Made Functional](https://pragprog.com/titles/swdddf/domain-modeling-made-functional/)

<!-- Local Variables: -->
<!-- fill-column: 80 -->
<!-- End: -->
