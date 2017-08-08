{ mkDerivation, attoparsec, base, bytestring, containers
, data-default-class, lens-family, parsec, pretty, stdenv, text
, transformers, void
}:
mkDerivation {
  pname = "proto-lens";
  version = "0.2.2.0";
  sha256 = "173sz83pw237qp037j6spy055ghayinfjg5m4p4mvgmjnnzpw1cj";
  libraryHaskellDepends = [
    attoparsec base bytestring containers data-default-class
    lens-family parsec pretty text transformers void
  ];
  homepage = "https://github.com/google/proto-lens";
  description = "A lens-based implementation of protocol buffers in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
