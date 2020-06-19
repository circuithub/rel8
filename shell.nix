{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;

  overrides = self: super: {
    base-compat = self.callPackage ./nix/base-compat.nix {};
    base-compat-batteries = self.callPackage ./nix/base-compat-batteries.nix {};
    ghc-check = self.callPackage ./nix/ghc-check.nix {};
    ghcide = self.callPackage ./nix/ghcide.nix {};
    haddock-library = self.callPackage ./nix/haddock-library.nix {};
    haskell-lsp = self.callPackage ./nix/haskell-lsp.nix {};
    haskell-lsp-types = self.callPackage ./nix/haskell-lsp-types.nix {};
    opaleye = self.callPackage ./nix/opaleye.nix {};
    optparse-applicative = self.callPackage ./nix/optparse-applicative.nix {};
    regex-base = self.callPackage ./nix/regex-base.nix {};
    regex-posix = self.callPackage ./nix/regex-posix.nix {};
    regex-tdfa = self.callPackage ./nix/regex-tdfa.nix {};
    time-compat = self.callPackage ./nix/time-compat.nix {};
    tmp-postgres = self.callPackage ./nix/tmp-postgres.nix {};
  };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackagesWithOverrides = haskellPackages.override { inherit overrides; };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackagesWithOverrides.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv.env
