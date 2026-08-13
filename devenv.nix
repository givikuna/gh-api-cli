{ pkgs, ... }: {
  languages = {
    haskell = {
      enable = true;
      package = pkgs.ghc;

      lsp = {
        enable = true;
        package = pkgs.haskell-language-server;
      };

      cabal = {
        enable = true;
        package = pkgs.cabal-install;
      };
    };
  };
}
