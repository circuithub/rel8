{ mkDerivation, aeson, array, async, base, base16-bytestring
, binary, bytestring, containers, cryptohash-sha1, data-default
, deepseq, directory, extra, filepath, fuzzy, ghc, ghc-boot
, ghc-boot-th, ghc-check, ghc-paths, ghc-typelits-knownnat, gitrev
, haddock-library, hashable, haskell-lsp, haskell-lsp-types
, hie-bios, hslogger, lens, lsp-test, mtl, network-uri
, optparse-applicative, parser-combinators, prettyprinter
, prettyprinter-ansi-terminal, QuickCheck, quickcheck-instances
, regex-tdfa, rope-utf16-splay, safe-exceptions, shake, sorted-list
, stdenv, stm, syb, tasty, tasty-expected-failure, tasty-hunit
, tasty-quickcheck, tasty-rerun, text, time, transformers, unix
, unordered-containers, utf8-string
}:
mkDerivation {
  pname = "ghcide";
  version = "0.2.0";
  sha256 = "fb38e2e0faa986b98acf2a8eb02549d92488ad0259ec087e47170f54c8904dfd";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base binary bytestring containers data-default
    deepseq directory extra filepath fuzzy ghc ghc-boot ghc-boot-th
    haddock-library hashable haskell-lsp haskell-lsp-types hslogger mtl
    network-uri prettyprinter prettyprinter-ansi-terminal regex-tdfa
    rope-utf16-splay safe-exceptions shake sorted-list stm syb text
    time transformers unix unordered-containers utf8-string
  ];
  executableHaskellDepends = [
    aeson async base base16-bytestring binary bytestring containers
    cryptohash-sha1 data-default deepseq directory extra filepath ghc
    ghc-check ghc-paths gitrev hashable haskell-lsp haskell-lsp-types
    hie-bios hslogger optparse-applicative shake text time
    unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory extra filepath ghc
    ghc-typelits-knownnat haddock-library haskell-lsp haskell-lsp-types
    lens lsp-test network-uri parser-combinators QuickCheck
    quickcheck-instances rope-utf16-splay shake tasty
    tasty-expected-failure tasty-hunit tasty-quickcheck tasty-rerun
    text
  ];
  doCheck = false;
  homepage = "https://github.com/digital-asset/ghcide#readme";
  description = "The core of an IDE";
  license = stdenv.lib.licenses.asl20;
}
