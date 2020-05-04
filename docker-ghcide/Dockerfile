FROM haskell:8.8

RUN cabal update
RUN git clone https://github.com/active-group/ghcide.git
RUN cd ghcide && cabal configure && cabal install
