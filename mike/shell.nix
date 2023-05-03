{ withVSCode ? false }:

let
  # Update the sources/pin with niv; see:
  # nix-shell -p niv --run 'niv update --help'
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { config.allowUnfree = withVSCode; };
  haskell-code =
    pkgs.haskellPackages.callCabal2nix "haskell-code" (pkgs.lib.cleanSource ./haskell-code) { };
  vscodeFunar = pkgs.vscode-with-extensions.override {
    vscodeExtensions = with pkgs.vscode-extensions; [
      bbenoist.nix
      haskell.haskell
      justusadam.language-haskell
    ];
  };
in pkgs.haskellPackages.shellFor {
  packages = _: [ haskell-code ];
  buildInputs = (with pkgs; [ cabal-install ghcid haskell-language-server ])
    ++ pkgs.lib.optional withVSCode vscodeFunar;
  shellHook = ''
    export PS1="\n\[\033[1;32m\][nix-shell:\W \[\033[1;31m\]FUNAR\[\033[1;32m\]]\$\[\033[0m\] "
    echo -e "\n\033[1;31m ♣ ♠ Welcome to FUNAR! ♥ ♦ \033[0m\n"
    echo -e "   Use the following command to open VSCode in this directory:\n"
    echo "       code ."
  '' + (if withVSCode then ''
    echo -e "\n   All required extensions should be pre-installed and ready."
  '' else
    "");
}
