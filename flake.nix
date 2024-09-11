{
  description = "FUNAR";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ];
      perSystem = { system, self', ... }:
        let
          # Pin GHC version for easier, explicit upgrades later
          ghcVersion = "948";
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
          formatter = pkgs.nixfmt;
          devShells = {
            default = pkgs.haskellPackages.shellFor {
              packages = _: [ hearts haskell-code ];
              nativeBuildInputs = [ pkgs.haskellPackages.doctest ];
              buildInputs = [
                pkgs.cabal-install
                pkgs.elmPackages.elm
                self'.packages.hls
                self'.formatter
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
            # we're able to globally install this GHC in the Docker image and
            # get rid of direnv as a dependency.
            ghcForFunar =
              builtins.head self.devShells.${system}.default.nativeBuildInputs;

            watch = pkgs.writeShellScriptBin "watch-and-commit" ''
              ${
                pkgs.lib.getExe pkgs.watch
              } -n 10 "git add . && git commit -m update && git push"
            '';
          };
        };
    };
}
