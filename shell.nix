{ nixpkgs ? import <nixpkgs> {} }:

let

  inherit (nixpkgs) pkgs;

  f = import ./.;

  haskellPackages =
    pkgs.haskell.packages.ghc802.override {
      overrides = self: super: {
        one-liner = super.callPackage /home/ollie/work/one-liner {};
        opaleye = pkgs.haskell.lib.dontCheck (super.callPackage /home/ollie/work/opaleye {});
      };
    };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
