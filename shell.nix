let

  ghcWithVersion = pkgs.haskell.compiler.ghcHEAD.override { version = "8.3.20170726"; };

  ghcVersion = pkgs.stdenv.lib.overrideDerivation ghcWithVersion (oldAttrs : {
                     name = "ghc-8.3.20170726";
                     src = pkgs.fetchgit {
                      url = "git://git.haskell.org/ghc.git";
                      rev = "1a9aae88ec8f57393db4992748a7de08c54cfe61";
                      sha256 = "06cjx9b8igmz9dh5zqfnpxy4ycq2wgbrayg9jnwvj1a08qbkzgd8";
                    };});

  pkgs = import <nixpkgs> {};
  plugin =
    onlyBuildHaskellPackages.callPackage ./default.nix {};
  ghc = onlyBuildHaskellPackages.ghcWithPackages (ps: [plugin]);

  onlyBuildHaskellPackages = pkgs.haskell.packages.ghc802.override {
     overrides = self: super: {
       mkDerivation = args: super.mkDerivation (args // {
         doHoogle = false;
         doCheck  = false;
         doHaddock = false;
          });
       ghc = ghcVersion;
       primitive = pkgs.haskell.lib.doJailbreak super.primitive;
       vector = pkgs.haskell.lib.doJailbreak super.vector;
       semigroupoids = super.callHackage "semigroupoids" "5.2" {} ;
       lens = super.callHackage "lens" "4.15.3" {} ;
       prettyprinter = null;

 	     haskell-indexer-backend-core = super.callPackage ./haskell-indexer/haskell-indexer-backend-core/default.nix {};
	     haskell-indexer-backend-ghc  =super.callPackage ./haskell-indexer/haskell-indexer-backend-ghc/default.nix {};
	     haskell-indexer-frontend-kythe  = super.callPackage ./haskell-indexer/haskell-indexer-frontend-kythe/default.nix {};
	     haskell-indexer-pathutil 	 = super.callPackage ./haskell-indexer/haskell-indexer-pathutil/default.nix {};
	     haskell-indexer-pipeline-ghckythe-wrapper  = super.callPackage ./haskell-indexer/haskell-indexer-pipeline-ghckythe-wrapper/default.nix {};
	     haskell-indexer-pipeline-ghckythe = super.callPackage ./haskell-indexer/pipeline-ghckythe/default.nix {};
	     haskell-indexer-translate  = pkgs.haskell.lib.doJailbreak (super.callPackage ./haskell-indexer/haskell-indexer-translate/default.nix {});
	     kythe-proto = super.callPackage ./haskell-indexer/text-offset/default.nix {};
	     kythe-schema  = super.callPackage ./haskell-indexer/text-offset/default.nix {};
	     text-offset 	= super.callPackage ./haskell-indexer/text-offset/default.nix {};

        };

      };
in
  pkgs.stdenv.mkDerivation {
    buildInputs = [ ghc ];
    name = "env";
  }
