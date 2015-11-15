[![Build Status](https://travis-ci.org/ftdebugger/haskell-labs.png)](https://travis-ci.org/ftdebugger/haskell-labs)

# Running tests

First make sure that all dependencies are installed:

```
$ cabal install --enable-tests
```

Then

## `cabal`

Just run

```
$ cabal build
$ cabal test
```

## Examples

Iris

```
$ ./dist/build/lab/lab ./dist/build/lab/lab -l -c 3 -i tasks/irises.txt
```

Butterfly

```
$ ./dist/build/lab/lab -c 2 -i tasks/butterfly.txt
```

Glass

```
$ ./dist/build/lab/lab -c 2 -i tasks/glass.txt
```

## Options

```
  -h --header         Ignore header,
  -f --first          Ignore first column,
  -l --last           Ignore last column,
  -e --euclid         Euclid distance,
  -m --hammin         Hamming distance,
  -c --cluster INT    Cluster count,
  -r --random         Random centers,
  -p --eps DOUBLE     EPS,
  -i --input STRING   Input file
```
