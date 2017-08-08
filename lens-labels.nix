{ mkDerivation, base, fetchgit, ghc-prim, stdenv }:
mkDerivation {
  pname = "lens-labels";
  version = "0.1.0.1";
  src = fetchgit {
    url = "https://github.com/google/proto-lens.git";
    sha256 = "14vxqipgbrjh6hjg2g4538h23bz1cdl0ldfw5axdi4371abl9gh5";
    rev = "120e83592e387bb1b28590168cec10164c1d16b9";
  };
  postUnpack = "sourceRoot+=/lens-labels/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base ghc-prim ];
  homepage = "https://github.com/google/proto-lens";
  description = "Integration of lenses with OverloadedLabels";
  license = stdenv.lib.licenses.bsd3;
}
