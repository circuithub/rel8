let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { overlays = [ ]; };
  hsPkgs = import ./default.nix;
in
hsPkgs.shellFor {
  withHoogle = false;
  tools = { cabal = "latest"; };
  exactDeps = false;
  buildInputs = [ pkgs.postgresql pkgs.pythonPackages.sphinx ];
}
