# Usage

Enter the environment with the plugin.

```
nix-shell
```

(This will take a while (1-2 hours) as it will build GHC).

To use the plugin.

```
ghc -fplugin KythePlugin -fplugin-opt KythePlugin:logs T.hs
./serve logs/ localhost:8080
```

Then access localhost:8080 in order to view the cross linked source.


# Examples

There are also some examples which can be rendered if kythe is installed.

```
./serve examples/ localhost:8080
```

Will render the output for 5 simple files from two projects.
