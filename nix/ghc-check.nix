{ mkDerivation, base, filepath, ghc, ghc-paths, process, stdenv
, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-check";
  version = "0.3.0.1";
  sha256 = "7f0f0dc6013891ab0a6edc9c3d7be0763d6126566b2cb064d36983f488c61da0";
  libraryHaskellDepends = [
    base filepath ghc ghc-paths process template-haskell transformers
  ];
  description = "detect mismatches between compile-time and run-time versions of the ghc api";
  license = stdenv.lib.licenses.bsd3;
}
