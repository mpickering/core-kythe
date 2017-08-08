{ mkDerivation, base, bytestring, containers, data-default-class
, lens-family, lens-labels, proto-lens, stdenv, text
}:
mkDerivation {
  pname = "proto-lens-descriptors";
  version = "0.2.2.0";
  sha256 = "1vjvr931ylnmpclizbrhqsx0x5jbmcbir0s53zpvm0f0vnwlwgqb";
  libraryHaskellDepends = [
    base bytestring containers data-default-class lens-family
    lens-labels proto-lens text
  ];
  description = "Protocol buffers for describing the definitions of messages";
  license = stdenv.lib.licenses.bsd3;
}
