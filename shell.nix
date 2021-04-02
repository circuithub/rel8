let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  hsPkgs = import ./default.nix;
  unstable = import sources.nixpkgs-unstable {};
in
  hsPkgs.shellFor {
    withHoogle = true;
    tools = { cabal = "latest"; };
    exactDeps = false;
    buildInputs = [ unstable.postgresql_13 pkgs.pythonPackages.sphinx ];
  }
