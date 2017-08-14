{ mkDerivation, base, bytestring, conduit, containers, filepath
, ghc, haskell-indexer-frontend-kythe, haskell-indexer-translate
, kythe-schema, lens, mmorph, mtl, prettyprinter, proto-lens
, stdenv, text
}:
mkDerivation {
  pname = "core-kythe";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit containers filepath ghc
    haskell-indexer-frontend-kythe haskell-indexer-translate
    kythe-schema lens mmorph mtl prettyprinter proto-lens text
  ];
  license = stdenv.lib.licenses.bsd3;
}
