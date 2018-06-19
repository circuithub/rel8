{ mkDerivation, adjunctions, aeson, base, bytestring, contravariant
, exceptions, free, lens, monad-control, mtl, one-liner, opaleye
, postgresql-simple, profunctors, resourcet, scientific, stdenv
, streaming, streaming-postgresql-simple, tagged, text, time
, transformers, unliftio, uuid, vector
}:
mkDerivation {
  pname = "rel8";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    adjunctions aeson base bytestring contravariant exceptions free
    lens monad-control mtl one-liner opaleye postgresql-simple
    profunctors resourcet scientific streaming
    streaming-postgresql-simple tagged text time transformers unliftio
    uuid vector
  ];
  testHaskellDepends = [ base ];
  description = "A type-safe, expressive and concise API for querying relational databases";
  license = stdenv.lib.licenses.bsd3;
}
