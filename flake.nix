{
  description = "Hasklean";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]hasklean \\[\\e[0;93m\\]\\w\\[\\e[0m\\]\\[\\e[0;94m\\]$(__git_ps1)\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  # inputs is an attribute set of all the dependencies of the flake
  inputs = {
    # latest packages list
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # tool that helps having standardized nix flake files
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

############################################################################

  # outputs is a function of one argument that takes an attribute set
  # of all the realized inputs, and outputs another attribute set
  # i.e. uses the inputs to build some outputs (packages, apps, shells,..)

  outputs = { self, nixpkgs, flake-parts }@inputs:

    # mkFlake is the main function of flake-parts to build a flake with standardized arguments
    flake-parts.lib.mkFlake { inherit self; inherit inputs; } {

      # list of systems to be built upon
      systems = nixpkgs.lib.systems.flakeExposed;

      # make a build for each system
      perSystem = { system, pkgs, lib, config, self', inputs', ... }: {

        packages = {
          # Haskell toolchain (GHC, cabal, HLS)
          ghc = pkgs.haskell.compiler.ghc8107;
          cabal-install = pkgs.cabal-install;
          haskell-language-server = pkgs.haskell-language-server.override {
            supportedGhcVersions = [ "8107" ];
          };
          # code editor
          vscodium = import ./nix/vscodium.nix { inherit pkgs; };
        };

        # Here is the definition of the nix-shell we use for development
        # It comes with all necessary packages + other nice to have tools
        devShells.default =
          let
            # tools for Haskell development
            devTools = [
              self'.packages.ghc
              self'.packages.cabal-install
              self'.packages.haskell-language-server
              self'.packages.vscodium
              # necessary for vscodium integrated terminal
              pkgs.bashInteractive
              # haskdogs and hasktags download and tag dependencies source files
              # so we can enjoy "jump to definition" in deps
              pkgs.haskellPackages.hasktags
              pkgs.haskellPackages.haskdogs
            ];
            # other useful tools
            otherTools = [
              pkgs.jq
            ];
          in
            pkgs.mkShell {
              buildInputs = devTools ++ otherTools;
              shellHook = ''
                source <(curl -s https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh);
                export LANG=C.utf8;
                cabal update;
                codium .
              '';
            };
      };
    };

############################################################################################

  # nixConfig is an attribute set of values which reflect the values given to nix.conf.
  # This can extend the normal behavior of a user's nix experience by adding flake-specific
  # configuration, such as a binary cache.
  nixConfig = {
    extra-substituers = [
      "https://cache.iog.io"
      "https://cache.nixos.org"
      "https://iohk.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
    allow-import-from-derivation = "true";
  };
}
