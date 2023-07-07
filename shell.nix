let
  sources = import ./nix/sources.nix;

  hsPkgs = import ./default.nix;

  haskellNix = import sources."haskell.nix" { };
  nixpkgsSrc = haskellNix.sources.nixpkgs-unstable;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  compiler-nix-name = "ghc943";

  haskell-nix = (import nixpkgsSrc nixpkgsArgs).haskell-nix;

  ch-hs-imports =
    let
      project = haskell-nix.project {
        src = sources.ch-hs-imports;
        inherit compiler-nix-name;
      };
    in
    project.hsPkgs.ch-hs-imports.components.exes.ch-hs-imports;

  pkgs = import sources.nixpkgs { overlays = [ ]; };
in
hsPkgs.shellFor {
  withHoogle = false;
  tools = { cabal = "latest"; fourmolu = "latest"; cabal-fmt = "latest"; };
  exactDeps = false;
  buildInputs = [ pkgs.nixpkgs-fmt pkgs.postgresql pkgs.treefmt ];
}
