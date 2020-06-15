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
    url = "git://github.com/circuithub/haskell-opaleye";
    sha256 = "04464lm4yzkc81jvcfxdxkdl9bb5wkkkkq0i5ny7s7l9vfwan4dp";
    rev = "3683bbabf98e6a65824dce02445e482c91fdbae2";
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
