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
        cabalProject = ''
          packages: .
          package haskell-language-server
            flags: -floskell -brittany
        '';
        modules = [{
          nonReinstallablePkgs = [
            "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
            "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
            # ghcjs custom packages
            "ghcjs-prim" "ghcjs-th"
            "ghc-bignum" "exceptions" "stm"
            "ghc-boot"
            "ghc" "Cabal" "Win32" "array" "binary" "bytestring" "containers"
            "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
            # "ghci" "haskeline"
            "hpc"
            "mtl" "parsec" "process" "text" "time" "transformers"
            "unix" "xhtml" "terminfo"
          ];
        }];
      };
    };
    exactDeps = false;
    buildInputs = [ pkgs.postgresql pkgs.pythonPackages.sphinx ];
  }
