let
  haskellNix = import (import ./nix/sources.nix)."haskell.nix" {};

  nixpkgsSrc = haskellNix.sources.nixpkgs-unstable;

  nixpkgsArgs = haskellNix.nixpkgsArgs;

  compiler-nix-name = "ghc941";

  pkgs = import nixpkgsSrc nixpkgsArgs;

in
pkgs.haskell-nix.project {
  inherit compiler-nix-name;

  cabalProjectLocal = builtins.readFile ./cabal.project.haskell-nix;

  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "rel8";
    src = ./.;
  };

  modules = [
    { packages.rel8 = {
        preCheck = ''
          export PATH="${pkgs.postgresql}/bin:${"$PATH"}"
        '';
      };
    }
  ];
}
