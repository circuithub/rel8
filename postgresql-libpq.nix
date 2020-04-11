{ mkDerivation, base, bytestring, Cabal, postgresql, stdenv, unix
}:
mkDerivation {
  pname = "postgresql-libpq";
  version = "0.9.4.2";
  sha256 = "cea053c79ef1505c30518db7b9fb2ee68c9e2915d48b22f01f8eb9a9b49f06f9";
  revision = "2";
  editedCabalFile = "1i0z3c0d657050kfggr5z8y4hmcqkckm2x1gn3fjbdzyx3p1rcaa";
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [ base bytestring unix ];
  librarySystemDepends = [ postgresql ];
  testHaskellDepends = [ base bytestring ];
  homepage = "https://github.com/phadej/postgresql-libpq";
  description = "low-level binding to libpq";
  license = stdenv.lib.licenses.bsd3;
}
