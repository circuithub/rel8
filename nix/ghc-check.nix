{ mkDerivation, base, containers, directory, filepath, ghc
, ghc-paths, process, stdenv, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-check";
  version = "0.4.0.0";
  sha256 = "243d3cd3b5cfe85ffb5dc24765c864dcf52928991296e407c4a52c8799f8a519";
  libraryHaskellDepends = [
    base containers directory filepath ghc ghc-paths process
    template-haskell transformers
  ];
  description = "detect mismatches between compile-time and run-time versions of the ghc api";
  license = stdenv.lib.licenses.bsd3;
}
