# Haskell Installation

- install [Visual Studio Code](https://code.visualstudio.com/)
- install [GHCup](https://www.haskell.org/ghcup/install/)
- select all "default" options
- answer "Do you want to install haskell-language-server (HLS)?" with "Yes"

Then run these commands:

```
ghcup install ghc 9.10.3
ghcup set ghc 9.10.3
```

To test the installation:

- start VSCode via  `code .` in this folder
- install the `haskell.haskell` extension
- in the file `haskell-code/Intro.hs`, delete a character in `where`
- after some time (it should show processing in the bottom bar), you
  should see red squiggles (not just red letters)

# Literature

- Sandy Maguire: [Algebra-Driven Design](https://leanpub.com/algebra-driven-design)
- Michael Pilquist, Rúnar Bjarnason, and Paul Chiusano [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala-second-edition)
- Scott Wlaschin: [Domain Modeling Made Functional](https://pragprog.com/titles/swdddf/domain-modeling-made-functional/)
- [Blog-Post on sums and products](https://funktionale-programmierung.de/en/2024/11/25/sums-products.html)
- [Haxl-Library für Parallelität mit Applicatives](https://github.com/facebook/Haxl)
- [SQLite-Library](https://hackage.haskell.org/package/sqlite-simple)

