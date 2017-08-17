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

  # Overloaded GHC version
  ghcWithVersion = pkgs.haskell.compiler.ghcHEAD.override { version = "8.3.20170726"; };

  ghcVersion = pkgs.stdenv.lib.overrideDerivation ghcWithVersion (oldAttrs : {
                     name = "ghc-8.3.20170726";
                     src = pkgs.fetchgit {
                      url = "git://git.haskell.org/ghc.git";
                      rev = "599aa0616211e42cf642a177515d5f8bee431eeb";
                      sha256 = "1izknlw0acds6znw6a69sqf6w2caa680jdf4ahimn89hyxnz6s7i";
                    };});

  # The main plugin, built with overridden package set
  plugin =
    onlyBuildHaskellPackages.callPackage ./default.nix {};

  # Overrides
  onlyBuildHaskellPackages = pkgs.haskell.packages.ghc802.override {
     overrides = self: super: {
       mkDerivation = args: super.mkDerivation (args // {
         doHoogle = false;
         doCheck  = false;
         doHaddock = false;
          });
       # New boot modules
       prettyprinter = null;
       text = null;


       # Over ride the GHC version to our custom one
       ghc = ghcVersion;

       # Specific Overrides
       primitive = pkgs.haskell.lib.doJailbreak super.primitive;
       vector = pkgs.haskell.lib.doJailbreak super.vector;
       semigroupoids = super.callHackage "semigroupoids" "5.2" {} ;
       lens = super.callHackage "lens" "4.15.3" {} ;
       lens-labels = super.callPackage ./lens-labels.nix {};
       proto-lens = pkgs.haskell.lib.addBuildTool (super.callPackage ./proto-lens.nix {}) pkgs.protobuf3_2;
       proto-lens-descriptors = pkgs.haskell.lib.addBuildTool (super.callPackage ./proto-lens-descriptors.nix {}) pkgs.protobuf3_2;
       proto-lens-protoc = pkgs.haskell.lib.addBuildTool (super.callPackage ./proto-lens-protoc.nix {}) pkgs.protobuf3_2;
       proto-lens-combinators = pkgs.haskell.lib.addBuildTool (super.callPackage ./proto-lens-combinators.nix {}) pkgs.protobuf3_2;

 	     haskell-indexer-backend-core = pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/haskell-indexer-backend-core/default.nix {});
	     haskell-indexer-backend-ghc  = pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/haskell-indexer-backend-ghc/default.nix {});
	     haskell-indexer-frontend-kythe  = pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/haskell-indexer-frontend-kythe/default.nix {});
	     haskell-indexer-pathutil 	 = super.callPackage ./haskell-indexer/haskell-indexer-pathutil/default.nix {};
	     haskell-indexer-pipeline-ghckythe-wrapper  = super.callPackage ./haskell-indexer/haskell-indexer-pipeline-ghckythe-wrapper/default.nix {};
	     haskell-indexer-pipeline-ghckythe = pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/haskell-indexer-pipeline-ghckythe/default.nix {});
	     haskell-indexer-translate  = pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/haskell-indexer-translate/default.nix {});
	     kythe-proto = pkgs.haskell.lib.addBuildTool (pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/kythe-proto/default.nix { kythe = pkgs.kythe; })) pkgs.protobuf3_2;
	     kythe-schema  = pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/kythe-schema/default.nix {});
	     text-offset 	= pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/text-offset/default.nix {});



        };

      };

  ghc = onlyBuildHaskellPackages.ghcWithPackages (ps: [plugin]);
in
  pkgs.stdenv.mkDerivation {
    buildInputs = [ ghc pkgs.kythe];
    shellHook = "export KYTHE_DIR=${pkgs.kythe}";
    name = "env";
  }
