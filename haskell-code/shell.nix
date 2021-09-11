{ pkgs ? import <nixpkgs> { } }:

let
  funar-code = pkgs.haskell.lib.doBenchmark
    (pkgs.haskellPackages.callCabal2nix "funar-code" ./. { });
in pkgs.haskellPackages.shellFor {
  packages = _: [ funar-code ];
  buildInputs = with pkgs; [ cabal-install ghc ghcid ormolu haskell-language-server ];
}
