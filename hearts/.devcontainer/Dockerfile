FROM debian:bullseye

ARG GHC_VERSION=8.10.7
ARG CABAL_VERSION=3.8.1.0
ARG HLS_VERSION=1.8.0.0
ARG HOME=/root

RUN apt-get update
RUN apt-get install -y --no-install-recommends curl ca-certificates build-essential libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 zlib1g-dev libnuma-dev llvm

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH="${HOME}/.cabal/bin:${HOME}/.ghcup/bin:$PATH"
RUN echo "export PATH=$PATH" >> ${HOME}/.profile

# Set the GHC version.
RUN ghcup install ghc ${GHC_VERSION} && ghcup set ghc ${GHC_VERSION}

RUN ghcup install cabal ${CABAL_VERSION} && ghcup set cabal ${CABAL_VERSION}

RUN cabal update

RUN ghcup install hls ${HLS_VERSION} && ghcup set hls ${HLS_VERSION}


