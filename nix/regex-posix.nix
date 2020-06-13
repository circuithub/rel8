{ mkDerivation, array, base, bytestring, containers, regex-base
, stdenv
}:
mkDerivation {
  pname = "regex-posix";
  version = "0.96.0.0";
  sha256 = "251300f1a6bb2e91abb8bf513a21981f8fab79c98a65acea2bb6d6a524414521";
  revision = "1";
  editedCabalFile = "1cy39n1928wv55i7k4wm7zd3xijk7p54kbrxxlfzfvgax5k163b9";
  libraryHaskellDepends = [
    array base bytestring containers regex-base
  ];
  description = "POSIX Backend for \"Text.Regex\" (regex-base)";
  license = stdenv.lib.licenses.bsd3;
}
