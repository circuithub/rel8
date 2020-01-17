{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, opaleye, stdenv }:
      mkDerivation {
        pname = "rel8";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base opaleye ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = (if compiler == "default"
                       then pkgs.haskellPackages
                     else pkgs.haskell.packages.${compiler}).override { inherit overrides; };

  overrides = self: super: {
    beam-core = self.callPackage ./beam-core.nix {};
    beam-postgres = self.callPackage ./beam-postgres.nix {};
    opaleye = self.callPackage ./opaleye.nix {};
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
