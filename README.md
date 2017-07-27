Usage

Enter the environment with the plugin.

```
nix-shell
```

(This will take a while as it will build GHC).

To use the plugin.

```
ghc -fplugin KythePlugin -fplugin-opt KythePlugin:output-dir T.hs
```

Will output the pretty printed source and currently the Haskell representation of
the kythe references.


