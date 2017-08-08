let
  pkgs = import <nixpkgs> {};

  # kythe 2.6
  kythe = pkgs.callPackage ./simple-kythe.nix {};

  # Overloaded GHC version
  ghcWithVersion = pkgs.haskell.compiler.ghcHEAD.override { version = "8.3.20170726"; };

  ghcVersion = pkgs.stdenv.lib.overrideDerivation ghcWithVersion (oldAttrs : {
                     name = "ghc-8.3.20170726";
                     src = pkgs.fetchgit {
                      url = "git://git.haskell.org/ghc.git";
                      rev = "1a9aae88ec8f57393db4992748a7de08c54cfe61";
                      sha256 = "06cjx9b8igmz9dh5zqfnpxy4ycq2wgbrayg9jnwvj1a08qbkzgd8";
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
	     kythe-proto = pkgs.haskell.lib.addBuildTool (pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/kythe-proto/default.nix { kythe = kythe; })) pkgs.protobuf3_2;
	     kythe-schema  = pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/kythe-schema/default.nix {});
	     text-offset 	= pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/text-offset/default.nix {});



        };

      };

  ghc = onlyBuildHaskellPackages.ghcWithPackages (ps: [plugin]);
in
  pkgs.stdenv.mkDerivation {
    buildInputs = [ ghc kythe];
    shellHook = "export KYTHE_DIR=${kythe}";
    name = "env";
  }
