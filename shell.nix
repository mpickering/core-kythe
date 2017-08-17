let
  pkgs = import <nixpkgs> {};

  # kythe 2.6
  kythe = pkgs.callPackage ./simple-kythe.nix {};

  # Overloaded GHC version
  ghcWithVersion = pkgs.haskell.compiler.ghcHEAD.override { version = "8.3.20170726"; };

  # Special build of GHC with annotated core
  ghc-pphead = pkgs.stdenv.lib.overrideDerivation ghcWithVersion (oldAttrs : {
                     name = "ghc-8.3.20170726";
                     src = pkgs.fetchgit {
                      url = "git://git.haskell.org/ghc.git";
                      rev = "599aa0616211e42cf642a177515d5f8bee431eeb";
                      sha256 = "1izknlw0acds6znw6a69sqf6w2caa680jdf4ahimn89hyxnz6s7i";
                    };});

  # The main plugin, built with overridden package set
  plugin =
    commonOverrides.callPackage ./default.nix {};

  # Common Overrides for building with HEAD, also has haskell-indexer packages
  commonOverrides = pkgs.haskell.packages.ghc802.override {
     overrides = self: super: {
       mkDerivation = args: super.mkDerivation (args // {
         doHoogle = false;
         doCheck  = false;
         doHaddock = false;
         doIndexer = true;
          });};

       # New boot modules
       #       prettyprinter = null;
       #text = null;

          # Over ride the GHC version to our custom one
          #ghc = ghc-pphead;
  };

  ghc-pphead-final =
    commonOverrides.override {
      overrides =
        self: super: {
          # New boot modules
          prettyprinter = null;
          text = null;

          # Over ride the GHC version to our custom one
          ghc = ghc-pphead;
        };
    };

  # The plugin which uses a custom version of HEAD
  ghc-core-kythe = commonOverrides.ghcWithPackages (ps: [plugin]);

  # Haskell indexer which uses ghc802
  ghc-haskell-indexer =
    commonOverrides.ghcWithPackages
      (ps : with ps;
        (callPackage ./haskell-indexer/default.nix {}).pkgs );

  indexer-test = commonOverrides.ghcWithIndexer (ps: [ps.profunctors]);
in
  pkgs.stdenv.mkDerivation {
      buildInputs = [ indexer-test kythe];
  #    buildInputs = [ ghc-core-kythe kythe];
    shellHook = "export KYTHE_DIR=${kythe}";
    name = "env";
  }
