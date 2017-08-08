{ stdenv, binutils , fetchurl }:

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
    cp -R ./ $out
  '';

  meta = with stdenv.lib; {
    description = "";
    homepage = http://developer.amd.com/tools/graphics-development/display-library-adl-sdk/;
    license = licenses.unfree;
    maintainers = [ maintainers.offline ];
    platforms = stdenv.lib.platforms.linux;
    hydraPlatforms = [];
  };
}
