{ nixpkgs ? import <nixpkgs> {} }:

let

  inherit (nixpkgs) pkgs;

  f = import ./.;

  haskellPackages =
    pkgs.haskell.packages.ghc802.override {
      overrides = self: super: {
        streaming-postgresql-simple = super.callPackage ../streaming-postgresql-simple {};

        opaleye = pkgs.haskell.lib.overrideCabal super.opaleye (drv: {
          src = pkgs.fetchurl {
            url = http://hackage.haskell.org/package/opaleye-0.6.0.0/opaleye-0.6.0.0.tar.gz;
            sha256 = "0prwlxp96qpnhdm34slwhp3j8hj961xl99xkl6fbrxgxxjngfg1q";
          };
          version = "0.6.0.0";
          doCheck = false;
          jailbreak = true;
        });
      };
 
    };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
