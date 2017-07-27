{ mkDerivation, base, filepath, ghc, haskell-indexer-translate
, lens, mtl, prettyprinter, stdenv, text
}:
mkDerivation {
  pname = "core-kythe";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base filepath ghc haskell-indexer-translate lens mtl prettyprinter
    text
  ];
  license = stdenv.lib.licenses.bsd3;
}
