# Lab 2 [![Build Status](https://travis-ci.org/ftdebugger/haskell-labs.png)](https://travis-ci.org/ftdebugger/haskell-labs)

## Running tests

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
$ ./dist/build/lab/lab ./dist/build/lab/lab -i tasks/irises.txt
```

Butterfly

```
$ ./dist/build/lab/lab -i tasks/butterfly.txt
```

Glass

```
$ ./dist/build/lab/lab -i tasks/glass.txt
```

## Options

```
  -h --header         Ignore header,
  -f --first          Ignore first column,
  -l --last           Ignore last column,
  -i --input STRING   Input file
  -o --output STRING  Output file
  -t --steps NUMBER   Number of iterations
  -s --trainSize Number Training set size. [0..1]
```
