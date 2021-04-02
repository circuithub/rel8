let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  hsPkgs = import ./default.nix;
in
  hsPkgs.shellFor {
    withHoogle = true;
    tools = { cabal = "latest"; };
    exactDeps = false;
    buildInputs = [ pkgs.postgresql pkgs.pythonPackages.sphinx ];
  }
