let
  # Update the sources/pin with niv; see:
  # nix-shell -p nix --run 'niv modify --help'
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  buildInputs = [ pkgs.elmPackages.elm ];
  shellHook = ''
    export PS1="\n\[\033[1;32m\][nix-shell:\W \[\033[1;31m\]FUNAR\[\033[1;32m\]]\$\[\033[0m\] "
    echo -e "\n\033[1;31m ♣ ♠ Welcome to FUNAR! ♥ ♦ \033[0m\n"
    echo -e "   Start the Elm frontend in this directory:\n"
    echo "       elm reactor"
  '';
}
