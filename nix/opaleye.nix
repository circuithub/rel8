{ mkDerivation, aeson, base, base16-bytestring, bytestring
, case-insensitive, containers, contravariant, dotenv, fetchgit
, hspec, hspec-discover, multiset, postgresql-simple, pretty
, product-profunctors, profunctors, QuickCheck, scientific
, semigroups, stdenv, text, time, time-locale-compat, transformers
, uuid, void
}:
mkDerivation {
  pname = "opaleye";
  version = "0.6.7004.2";
  src = fetchgit {
    url = "https://github.com/tomjaguarpaw/haskell-opaleye";
    sha256 = "1xwyp2gk31zj3i6zr3nf0ljzds0rz4zvv5qgqrz2d6pilk5ydz7m";
    rev = "6caea6edecc3fad2ce30d9b37897d1a6caea1106";
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
