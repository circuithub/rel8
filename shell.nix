let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  hsPkgs = import ./default.nix;
in
  hsPkgs.shellFor {
    withHoogle = false;
    tools = { cabal = "latest"; };
  }
