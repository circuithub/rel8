let pkgs = (builtins.getFlake "nixpkgs").legacyPackages.x86_64-linux;
in
  pkgs.mkShell { buildInputs = with pkgs; [ghc cabal-install postgresql postgresql.dev zlib
  pkg-config];}
