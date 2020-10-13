let pkgs = import ./nix/pkgs.nix;
in { rel8 = pkgs.haskellPackages.rel8; }
