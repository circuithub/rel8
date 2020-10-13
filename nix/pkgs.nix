import (builtins.fetchTarball {
  name = "nixos-20.03";
  url = "https://github.com/nixos/nixpkgs/archive/0d0660fde3bb53a3d013b65e5e141eb11d1efb82.tar.gz";
  sha256 = "13qpa916qq1kqvfj8q4zkmnfnbh2kpx0nxxg04nblai0smz97820";
}) {
  overlays = import ./overlays.nix;
}
