{
  description = "gh-api-cli";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        gh-api-cli = pkgs.haskellPackages.callCabal2nix "gh-api-cli" ./. { };
      in
      {
        packages.default = gh-api-cli;

        apps.default = flake-utils.lib.mkApp {
          drv = gh-api-cli;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ gh-api-cli.env ];

          buildInputs = with pkgs; [
            ghc
            haskell-language-server
            cabal-install
            stylish-haskell
            haskellPackages.cabal-gild
            zlib
            pkg-config
          ];
        };
      }
    );
}
