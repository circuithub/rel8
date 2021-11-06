let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  hsPkgs = import ./default.nix;
in
  hsPkgs.shellFor {
    withHoogle = false;
    tools = { 
      cabal = "latest"; 
      haskell-language-server = {
        version = "latest"; 
        modules = [{ packages.haskell-language-server.flags.floskell = false; }];
      };
    };
    exactDeps = false;
    buildInputs = [ pkgs.postgresql pkgs.pythonPackages.sphinx ];
  }
