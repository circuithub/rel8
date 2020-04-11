{ mkDerivation, base, bytestring, ghc-prim, hspec, hspec-discover
, mtl, stdenv, template-haskell, th-lift, th-lift-instances
, th-reify-many
}:
mkDerivation {
  pname = "th-orphans";
  version = "0.13.9";
  sha256 = "3f3c6bead037cb11faf633b3938d1af2c6b11399e8353b72e80d9eb3b57e41f6";
  revision = "1";
  editedCabalFile = "100gpi0np50vxlapyl6d05w8ss9l2gqacn28i228hsvhvrxxwjdy";
  libraryHaskellDepends = [
    base mtl template-haskell th-lift th-lift-instances th-reify-many
  ];
  testHaskellDepends = [
    base bytestring ghc-prim hspec template-haskell th-lift
  ];
  testToolDepends = [ hspec-discover ];
  description = "Orphan instances for TH datatypes";
  license = stdenv.lib.licenses.bsd3;
}
