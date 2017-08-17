{ stdenv, binutils , fetchurl, glibc }:

stdenv.mkDerivation rec {
  version = "0.0.26";
  name = "kythe-${version}";

  src = fetchurl {
    url = "https://github.com/google/kythe/releases/download/v0.0.26/kythe-v0.0.26.tar.gz";
    sha256 = "0dij913fjymrbdlfjnr65ib90x5xd3smia3bh83q9prh7sfi5h07";
  };

  buildInputs =
    [ binutils ];


  doCheck = false;




  buildPhase = ''
  '';

  testPhase = ''
  '';

  installPhase = ''
    patchelf --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" tools/write_entries
    patchelf --set-rpath "${stdenv.cc.cc.lib}/lib64" tools/write_entries
    patchelf --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" tools/write_tables
    patchelf --set-rpath "${stdenv.cc.cc.lib}/lib64" tools/write_tables
    patchelf --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" tools/http_server
    patchelf --set-rpath "${stdenv.cc.cc.lib}/lib64" tools/http_server
    cp -R ./ $out
  '';

  meta = with stdenv.lib; {
    description = "";
  };
}
