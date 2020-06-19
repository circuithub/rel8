{ mkDerivation, aeson, base, bytestring, case-insensitive, hedgehog
, lifted-base, monad-control, opaleye, postgresql-simple
, product-profunctors, profunctors, scientific, semigroupoids
, stdenv, tasty, tasty-hedgehog, text, time, tmp-postgres, uuid
}:
mkDerivation {
  pname = "rel8";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive opaleye postgresql-simple
    product-profunctors profunctors scientific semigroupoids text time
    uuid
  ];
  testHaskellDepends = [
    base bytestring case-insensitive hedgehog lifted-base monad-control
    postgresql-simple scientific tasty tasty-hedgehog text time
    tmp-postgres uuid
  ];
  description = "Hey! Hey! Can u rel8?";
  license = stdenv.lib.licenses.bsd2;
}
