let
  # Update the sources/pin with niv; see:
  # nix-shell -p nix --run 'niv modify --help'
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  funar-code =
    pkgs.haskellPackages.callCabal2nix "funar-code" (pkgs.lib.cleanSource ./.)
    { };
in pkgs.haskellPackages.shellFor {
  packages = _: [ funar-code ];
  buildInputs = with pkgs; [ cabal-install ghcid haskell-language-server ];
}
