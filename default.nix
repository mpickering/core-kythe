{ mkDerivation, base, conduit, filepath, ghc
, haskell-indexer-frontend-kythe, haskell-indexer-translate, lens
, mmorph, mtl, prettyprinter, stdenv, text
}:
mkDerivation {
  pname = "core-kythe";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base conduit filepath ghc haskell-indexer-frontend-kythe
    haskell-indexer-translate lens mmorph mtl prettyprinter text
  ];
  license = stdenv.lib.licenses.bsd3;
}
