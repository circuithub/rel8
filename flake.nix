{
  description = "rel8";

  nixConfig = {
    extra-substituters = [
      "https://rel8.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "rel8.cachix.org-1:C/hVFTPHIOp7S/jRleuLL0Cg/dE0dQzEuEB5szaszTc="
    ];
  };

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    haskellNix.url = github:input-output-hk/haskell.nix;
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };

        rel8 = pkgs.haskell-nix.project {
          compiler-nix-name = "ghc963";

          cabalProjectLocal = builtins.readFile ./cabal.project.haskell-nix;

          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "rel8";
            src = ./.;
          };

          modules = [
            {
              packages.rel8 = {
                preCheck = ''
                  export PATH="${pkgs.postgresql}/bin:${"$PATH"}"
                '';
              };
            }
          ];
        };
      in
      {
        checks.tests = rel8.hsPkgs.rel8.checks.tests;

        packages.default = rel8.hsPkgs.rel8.components.library;

        devShells.default = rel8.shellFor {
          withHoogle = false;
          tools = { cabal = "latest"; };
          exactDeps = false;
          buildInputs = [ pkgs.postgresql pkgs.scriv ];
        };
      }
    );
}
