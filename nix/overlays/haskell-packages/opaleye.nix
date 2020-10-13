{ mkDerivation, aeson, base, base16-bytestring, bytestring
, case-insensitive, containers, contravariant, dotenv, hspec
, hspec-discover, multiset, postgresql-simple, pretty
, product-profunctors, profunctors, QuickCheck, scientific
, semigroups, stdenv, text, time, time-locale-compat, transformers
, uuid, void
}:
mkDerivation {
  pname = "opaleye";
  version = "0.7.1.0";
  sha256 = "c65faccdc01207d94500198b3ec33db7daaa25daae28d47f234b9ad6926f9644";
  revision = "3";
  editedCabalFile = "14y8nnng9307wb1mafzdr2fmn37cwyfpw9sby8lf9sj467rvghrq";
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring case-insensitive
    contravariant postgresql-simple pretty product-profunctors
    profunctors scientific semigroups text time time-locale-compat
    transformers uuid void
  ];
  testHaskellDepends = [
    aeson base bytestring containers contravariant dotenv hspec
    hspec-discover multiset postgresql-simple product-profunctors
    profunctors QuickCheck semigroups text time transformers uuid
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "https://github.com/tomjaguarpaw/haskell-opaleye";
  description = "An SQL-generating DSL targeting PostgreSQL";
  license = stdenv.lib.licenses.bsd3;
}
