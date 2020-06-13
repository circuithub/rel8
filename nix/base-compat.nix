{ mkDerivation, base, stdenv, unix }:
mkDerivation {
  pname = "base-compat";
  version = "0.11.1";
  sha256 = "356f1542ec93e7192a9073d32a8ef1e48194275495dcec7fb0d241f4baaf6d36";
  libraryHaskellDepends = [ base unix ];
  description = "A compatibility layer for base";
  license = stdenv.lib.licenses.mit;
}
