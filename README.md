There are two parts in this repo currently.

The first is a plugin which prints out core and emits kythe cross-links for it.

The second is a nix function which recursively indexes and creates tables
for all the dependencies of a project.

# Plugin Usage

Enter the environment with the plugin.

```
nix-shell
```

(This will take a while (1-2 hours) as it will build GHC).

To use the plugin.

```
ghc -fplugin KythePlugin -fplugin-opt KythePlugin:logs T.hs
./serve.sh logs/ localhost:8080
```

Then access localhost:8080 in order to view the cross linked source.

# ghcWithIndexer

```
nix-shell haskell-with-indexer.nix
serve
```

Will build the `conduit` package by default and cross-link all its dependencies.
There are some problems with the resulting directory structure but everything
fundamentally works.

You can modify which package is built by changing `ps.conduit` in the nix file.


# Examples

There are also some examples which can be rendered if kythe is installed.

```
./serve examples/ localhost:8080
```

Will render the output for 5 simple files from two projects.
