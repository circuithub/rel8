{ mkDerivation, aeson, attoparsec, base, base16-bytestring
, bytestring, bytestring-builder, case-insensitive, containers
, cryptohash-md5, filepath, hashable, HUnit, inspection-testing
, Only, postgresql-libpq, scientific, stdenv, tasty, tasty-golden
, tasty-hunit, template-haskell, text, time, transformers
, uuid-types, vector
}:
mkDerivation {
  pname = "postgresql-simple";
  version = "0.6.2";
  sha256 = "fbfaf2b326d5d43798f60b25862a9ac5006c0074fe295ba523fb55443a75f396";
  revision = "3";
  editedCabalFile = "0wngpd4187sfz2jhpnnrycfvpd1irz2m798p515mn1ryg1ywmhbr";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring bytestring-builder
    case-insensitive containers hashable Only postgresql-libpq
    scientific template-haskell text time transformers uuid-types
    vector
  ];
  testHaskellDepends = [
    aeson base base16-bytestring bytestring case-insensitive containers
    cryptohash-md5 filepath HUnit inspection-testing postgresql-libpq
    tasty tasty-golden tasty-hunit text time vector
  ];
  benchmarkHaskellDepends = [ base vector ];
  doCheck = false;
  description = "Mid-Level PostgreSQL client library";
  license = stdenv.lib.licenses.bsd3;
}
