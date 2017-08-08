{ mkDerivation, base, Cabal, data-default-class, HUnit, lens-family
, lens-family-core, proto-lens, proto-lens-protoc, stdenv
, test-framework, test-framework-hunit, transformers
}:
mkDerivation {
  pname = "proto-lens-combinators";
  version = "0.1.0.8";
  sha256 = "0byz61d1xd1khksvh170q7a7qvziigxf76ngcsd650fahqaardzz";
  setupHaskellDepends = [ base Cabal proto-lens-protoc ];
  libraryHaskellDepends = [
    base data-default-class lens-family proto-lens proto-lens-protoc
    transformers
  ];
  testHaskellDepends = [
    base HUnit lens-family lens-family-core proto-lens
    proto-lens-protoc test-framework test-framework-hunit
  ];
  homepage = "https://github.com/google/proto-lens";
  description = "Utilities functions to proto-lens";
  license = stdenv.lib.licenses.bsd3;
}
