{ pkgs ? import <nixpkgs> { } }:

let
  hearts = pkgs.haskell.lib.doBenchmark
    (pkgs.haskellPackages.callCabal2nix "hearts" ./. { });
in pkgs.haskellPackages.shellFor {
  packages = _: [ hearts ];
  buildInputs = with pkgs; [ cabal-install ghc ghcid ormolu ];
}
