let
  _nixpkgs = import <nixpkgs> {};
in
{ nixpkgs ? import (_nixpkgs.fetchFromGitHub { owner = "mpickering"
                                              ; repo  = "nixpkgs"
                                              ; rev   = "9d18805ba130d90e8f11023535d8fab778a40ca3"
                                              ; sha256 = "0y1mz55fwbxvfg826m2hykc7sir3j0xpgcdifwbdy1cz2wnc99xy";}) {}
  }:
let
  pkgs = if nixpkgs == null then _nixpkgs else nixpkgs;

  # The main plugin, built with overridden package set
  plugin =
    commonOverrides.callPackage ./default.nix {};

  # Common Overrides for building with HEAD, also has haskell-indexer packages
  commonOverrides = pkgs.haskell.packages.ghc802.override {
     overrides = self: super: {
       mkDerivation = args: super.mkDerivation (args // {
        doIndexer = true;
          });
  };};

  indexer-test = commonOverrides.ghcWithIndexer (ps: [ps.conduit]);
in
  pkgs.stdenv.mkDerivation {
    # Run with the serve command
    buildInputs = [ indexer-test pkgs.kythe];
    shellHook = "export KYTHE_DIR=${pkgs.kythe}";
    name = "env";
  }
