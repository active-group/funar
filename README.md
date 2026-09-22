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


