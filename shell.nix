{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, opaleye, stdenv, tasty, tasty-hedgehog, tasty-hunit, tmp-postgres }:
      mkDerivation {
        pname = "rel8";
        version = "0.1.0.0";
        src = ./.;
        buildTools = [ pkgs.postgresql_11 ];
        libraryHaskellDepends = [ base opaleye ];
        testHaskellDepends = [ tasty tasty-hedgehog tasty-hunit tmp-postgres ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = (if compiler == "default"
                       then pkgs.haskellPackages
                     else pkgs.haskell.packages.${compiler}).override { inherit overrides; };

  overrides = self: super: {
    opaleye = self.callPackage ./opaleye.nix {};
    tmp-postgres = self.callPackage ./tmp-postgres.nix {};
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv.env
