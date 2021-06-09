#!/bin/sh
mkdir -p .cabal
docker run --rm -p 8080:8080 -p 8001:8001 -p 8002:8002 -p 8003:8003 -p 8004:8004 -v `pwd`:/hearts -v `pwd`/.cabal:/root/.cabal -w /hearts -i -t haskell:8.10 bash
