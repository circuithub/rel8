{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc881", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, adjunctions, base, distributive, opaleye
      , postgresql-simple, profunctors, record-dot-preprocessor
      , record-hasfield, singletons, stdenv, transformers
      }:
      mkDerivation {
        pname = "rel8";
        version = "1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          adjunctions base distributive opaleye postgresql-simple profunctors
          record-dot-preprocessor record-hasfield singletons transformers
        ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages.override overrides
                       else pkgs.haskell.packages.${compiler}.override overrides;

  overrides = {
    overrides = self: super: {
      opaleye = self.callPackage ./opaleye.nix {};
      singletons = self.callPackage ./singletons.nix {};
      postgresql-libpq = self.callPackage ./postgresql-libpq.nix {};
      th-desugar = self.callPackage ./th-desugar.nix {};
      th-orphans = self.callPackage ./th-orphans.nix {};
      postgresql-simple = self.callPackage ./postgresql-simple.nix {};
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

drv.env
