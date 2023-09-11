{
  description = "FUNAR";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, ... }:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        # Pin GHC version for easier, explicit upgrades later
        ghcVersion = "927";
        pkgs = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            (_: prev: {
              haskellPackages = prev.haskell.packages."ghc${ghcVersion}";
            })
          ];
        };
        hearts = pkgs.haskellPackages.callCabal2nix "hearts"
          (pkgs.lib.cleanSource ./hearts) { };
        haskell-code = pkgs.haskellPackages.callCabal2nix "haskell-code"
          (pkgs.lib.cleanSource ./haskell-code) { };
      in {
        devShells = {
          default = pkgs.haskellPackages.shellFor {
            packages = _: [ hearts haskell-code ];
            buildInputs = [
              pkgs.cabal-install
              self.packages.${system}.hls
              pkgs.elmPackages.elm
            ];
            shellHook = ''
              export PS1="\n\[\033[1;32m\][nix-shell:\W \[\033[1;31m\]FUNAR\[\033[1;32m\]]\$\[\033[0m\] "
              echo -e "\n\033[1;31m ♣ ♠ Welcome to FUNAR! ♥ ♦ \033[0m\n"
              echo -e "   Use the following command to open VSCode in this directory:\n"
              echo "       code ."
            '';
          };

          withVSCode = self.devShells.${system}.default.overrideAttrs (old:
            let
              vscode = pkgs.vscode-with-extensions.override {
                vscodeExtensions = with pkgs.vscode-extensions; [
                  bbenoist.nix
                  haskell.haskell
                  justusadam.language-haskell
                ];
              };
            in {
              buildInputs = old.buildInputs ++ [ vscode ];
              shellHook = old.shellHook + ''
                echo -e "\n   All required extensions should be pre-installed and ready."'';
            });
        };

        packages = {
          inherit hearts haskell-code;
          inherit (pkgs) cabal-install;
          hls = pkgs.haskell-language-server.override {
            supportedGhcVersions = [ ghcVersion ];
          };
          # HACK: We rely on how `shellFor` constructs its `nativeBuildInputs`
          # in order to grab the `ghcWithPackages` from out of there. That way
          # we're able to globally install this GHC in the Docker image and get
          # rid of direnv as a dependency.
          ghcForFunar =
            builtins.head self.devShells.${system}.default.nativeBuildInputs;
        };
      });
}
