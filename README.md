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
$ ./dist/build/lab/lab ./dist/build/lab/lab -l -c 4 -i tasks/irises.txt
```
