{ mkDerivation, base, bytestring, Cabal, containers
, data-default-class, directory, filepath, haskell-src-exts
, lens-family, lens-labels, process, proto-lens
, proto-lens-descriptors, stdenv, text
}:
mkDerivation {
  pname = "proto-lens-protoc";
  version = "0.2.2.3";
  sha256 = "08s93h25l66z7w45jmy632lhhkddqarj94bpwn3wmv5kdpsp33pq";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring Cabal containers data-default-class directory
    filepath haskell-src-exts lens-family lens-labels process
    proto-lens proto-lens-descriptors text
  ];
  executableHaskellDepends = [
    base bytestring containers data-default-class filepath
    haskell-src-exts lens-family proto-lens proto-lens-descriptors text
  ];
  description = "Protocol buffer compiler for the proto-lens library";
  license = stdenv.lib.licenses.bsd3;
}
