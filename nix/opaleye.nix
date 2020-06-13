{ mkDerivation
, aeson
, base
, base16-bytestring
, bytestring
, case-insensitive
, containers
, contravariant
  #, dotenv
, fetchgit
, hspec
, hspec-discover
, multiset
, postgresql-simple
, pretty
, product-profunctors
, profunctors
, QuickCheck
, scientific
, semigroups
, stdenv
, text
, time
, time-locale-compat
, transformers
, uuid
, void
}:
mkDerivation {
  pname = "opaleye";
  version = "0.6.7004.2";
  src = fetchgit {
    url = "https://github.com/circuithub/haskell-opaleye";
    sha256 = "1zmdxqikzvad035c75z02whg0jvs0g3i9zc3rj97xzb2q6ay4m3r";
    rev = "430dd995f365e68d05aa7ed9f8affec2e0f13b50";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson
    base
    base16-bytestring
    bytestring
    case-insensitive
    contravariant
    postgresql-simple
    pretty
    product-profunctors
    profunctors
    scientific
    semigroups
    text
    time
    time-locale-compat
    transformers
    uuid
    void
  ];
  # dotenv, which is needed for the test suite, is broken in our nixpkgs,
  # so disable testing for now
  doCheck = false;
  testHaskellDepends = [
    aeson
    base
    containers
    contravariant
    # dotenv
    hspec
    hspec-discover
    multiset
    postgresql-simple
    product-profunctors
    profunctors
    QuickCheck
    semigroups
    text
    time
    transformers
    uuid
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/tomjaguarpaw/haskell-opaleye";
  description = "An SQL-generating DSL targeting PostgreSQL";
  license = stdenv.lib.licenses.bsd3;
}
