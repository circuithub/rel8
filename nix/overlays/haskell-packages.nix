self:
super:

let
  inherit (self) haskell haskellPackages;

  inherit (haskellPackages) callCabal2nix;

  inherit (haskell.lib) addBuildTool appendConfigureFlag packagesFromDirectory;

  inherit (super.lib) composeExtensions;

  pkgs = self;

  WError =
    drv: appendConfigureFlag drv "--ghc-option=-Werror";

  configurations =
    self: super: {
      rel8 = addBuildTool (WError (callCabal2nix "rel8" (builtins.path { path = ../../.; name = "rel8"; }) {})) pkgs.postgresql;
    };

in
{
  haskellPackages =
    super.haskellPackages.override
      (
        old:
          {
            overrides =
              composeExtensions
                (old.overrides or (_: _: {}))
                (
                  composeExtensions
                    (packagesFromDirectory { directory = ./haskell-packages; })
                    configurations
                );
          }
      );
}
