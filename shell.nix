{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ghcide, opaleye, stdenv }:
      mkDerivation {
        pname = "rel8";
        version = "1.0.0";
        src = pkgs.nix-gitignore.gitignoreSource [] ./.;
        libraryHaskellDepends = [ base ghcide opaleye ];
        doHaddock = false;
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  overrides = self: super: {
    base-compat = self.callPackage ./nix/base-compat.nix {};
    base-compat-batteries = self.callPackage ./nix/base-compat-batteries.nix {};
    ghc-check = self.callPackage ./nix/ghc-check.nix {};
    ghcide = self.callPackage ./nix/ghcide.nix {};
    haddock-library = self.callPackage ./nix/haddock-library.nix {};
    haskell-lsp = self.callPackage ./nix/haskell-lsp.nix {};
    haskell-lsp-types = self.callPackage ./nix/haskell-lsp-types.nix {};
    hie-bios = self.callPackage ./nix/hie-bios.nix {};
    opaleye = self.callPackage ./nix/opaleye.nix {};
    optparse-applicative = self.callPackage ./nix/optparse-applicative.nix {};
    regex-base = self.callPackage ./nix/regex-base.nix {};
    regex-posix = self.callPackage ./nix/regex-posix.nix {};
    regex-tdfa = self.callPackage ./nix/regex-tdfa.nix {};
    time-compat = self.callPackage ./nix/time-compat.nix {};
  };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackagesWithOverrides = haskellPackages.override { inherit overrides; };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackagesWithOverrides.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv.env
