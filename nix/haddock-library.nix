{ mkDerivation, base, base-compat, bytestring, containers, deepseq
, directory, filepath, hspec, hspec-discover, optparse-applicative
, parsec, QuickCheck, stdenv, text, transformers, tree-diff
}:
mkDerivation {
  pname = "haddock-library";
  version = "1.9.0";
  sha256 = "ac3032d3e2ba87f69c8207b29966e5cda023a30dd25b4d6ae14a93bc2bac6730";
  libraryHaskellDepends = [
    base bytestring containers parsec text transformers
  ];
  testHaskellDepends = [
    base base-compat bytestring containers deepseq directory filepath
    hspec optparse-applicative parsec QuickCheck text transformers
    tree-diff
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://www.haskell.org/haddock/";
  description = "Library exposing some functionality of Haddock";
  license = stdenv.lib.licenses.bsd2;
}
