{ mkDerivation, aeson, base, base16-bytestring, bytestring
, case-insensitive, containers, contravariant, dotenv, fetchgit
, hspec, hspec-discover, multiset, postgresql-simple, pretty
, product-profunctors, profunctors, QuickCheck, scientific
, semigroups, stdenv, text, time, time-locale-compat, transformers
, uuid, void
}:
mkDerivation {
  pname = "opaleye";
  version = "0.6.7004.1";
  src = fetchgit {
    url = "https://github.com/tomjaguarpaw/haskell-opaleye";
    sha256 = "0721wb66q1b5bal0wah6sz7ak7z3r93q7yvkh794gw4n6r7ffipq";
    rev = "82226cd0d38c28f1dc515eb5524045307141ebb0";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring case-insensitive
    contravariant postgresql-simple pretty product-profunctors
    profunctors scientific semigroups text time time-locale-compat
    transformers uuid void
  ];
  testHaskellDepends = [
    aeson base containers contravariant dotenv hspec hspec-discover
    multiset postgresql-simple product-profunctors profunctors
    QuickCheck semigroups text time transformers uuid
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "https://github.com/tomjaguarpaw/haskell-opaleye";
  description = "An SQL-generating DSL targeting PostgreSQL";
  license = stdenv.lib.licenses.bsd3;
}
