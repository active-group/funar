# monolith

* `stack new monolith`
* `cd monolith`

* `vim package.yaml`
``` .yaml
library:
  source-dirs: src
  dependencies:
    - mtl
    - polysemy
```

* `vim app/Main.hs`
``` .hs
import Lib
import Polysemy
```

* `stack build --file-watch --fast`

# Visual Studio Code

## Fancy

```hs
addition a b = a + b
```
↓
```hs
addition :: Num a => a -> a -> a
addition a b = a + b
```

* `-- >>> succ 42` → `43`

* https://gitlab.haskell.org/zander/haskell_set-up/-/wikis/linux_fancy

### ghcid

* `stack install ghcid`

### ghcide

* `cd ~/Downloads`
* `git clone https://github.com/digital-asset/ghcide.git`
* `cd ghcide`
* ~~`mv stack88.yaml stack.yaml`~~
  * there is no `stack88.yaml` :/
* edit `stack.yaml`
  * https://www.stackage.org/lts
    * copy current resolver, e.g. `lts-16.25`
    * paste current resolver under `resolver`
* `stack install`

### Set Up

* https://github.com/haskell/ghcide#using-with-vs-code
  * `Ctrl-P` + `ext install haskell.haskell`
* edit `stack.yaml`
  * update `resolver`, e.g. `url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/16/25.yaml`

## Hoogle

```hs
Num a => a -> a -> a
```
↓
```hs
(+)
```

* https://marketplace.visualstudio.com/items?itemName=jcanero.hoogle-vscode
  * `Ctrl-P` + `ext install jcanero.hoogle-vscode`
  * `Alt-Shift-H`

## Pointfree

```hs
addition a b = a + b
```
↓
```hs
addition = (+)
```

 * `stack install pointfree`
 * `stack install pointful`
   * add to `extra-deps` in `stack.yaml`, e.g.:
     * `haskell-src-exts-simple-1.21.1.0@sha256:b2f5b16263753a16d8462faf9d100812a864eaae41eec803cfd32f3ed7b18155,1578`
     * `haskell-src-exts-1.21.1@sha256:11d18ec3f463185f81b7819376b532e3087f8192cffc629aac5c9eec88897b35,4541`

 * https://marketplace.visualstudio.com/items?itemName=JoseDanielD.haskell-pointfree
   * `Ctrl-P` + `ext install JoseDanielD.haskell-pointfree`
   * "To use just select some code, right click the selected code and pick "Pointfree" from context menu."

## Prettify Symbols

```hs
addition :: Num alpha => alpha -> alpha -> alpha
```
↓
```hs
addition ∷ Num α ⇒ α → α → α
```

* https://marketplace.visualstudio.com/items?itemName=siegebell.prettify-symbols-mode
  * `Ctrl-P` + `ext install siegebell.prettify-symbols-mode`
* `Ctrl-,` + `Substitutions`
  * click `Edit in settings.json`

```json
                {
                    "ugly": "=>",
                    "pretty": "⇒"
                },
                {
                    "ugly": "::",
                    "pretty": "∷"
                },                {
                    "ugly": "Bool",
                    "pretty": "𝔹"
                },
```

## WAI - Web Application Interface

* https://hackage.haskell.org/package/wai

* `vim package.yaml`
``` .yaml
dependencies:
- wai
- warp
- http-types
```

# Literature

Tagles Final:
* https://serokell.io/blog/tagless-final
* https://jproyo.github.io/posts/2019-03-17-tagless-final-haskell.html

Polysemy:
* https://hackage.haskell.org/package/polysemy

Clean Architecture:
* https://github.com/thma/PolysemyCleanArchitecture

Events:
* https://blog.jayway.com/2013/06/20/dont-publish-domain-events-return-them/

Polysemy is fun!:
* https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1/
* https://haskell-explained.gitlab.io/blog/posts/2019/07/31/polysemy-is-cool-part-2/

DDD-Architekturen im Vergleich:
* https://www.maibornwolff.de/blog/ddd-architekturen-im-vergleich

Why WAI? = Your First Haskell Web App With WAI And Warp by Michael Snoyman
* https://www.youtube.com/watch?v=mz5_HmLGRXc

* Common packages:
  * `wai`
  * `warp`
  * `wai-extra`
  * `wai-conduit`
  * `pipes-wai`
  * `wai-websockets`

Event Bus in Haskell
* https://stackoverflow.com/questions/32434339/how-to-write-an-event-bus-in-haskell

Comonads
* http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html
