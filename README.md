# Formulative

(Caution: This package is experimental and under construction.)

`Formulative` is an open-source package for polymorphic numerical simulation written in Haskell. It provides unified syntax for mathematical operators appliable to multiple types such as scalar, vector, matrix, and data type combining these types.

Examples are in [formulative-examples](formulative-examples).

## Setup

If you don't have GHC, use GHCup (https://www.haskell.org/ghcup/) and install GHC, cabal, HLS, and stack (HLS and stack are not necessary to build package, but useful for development).

## Build

There are two ways to build the package.

1. Using cabal (recommended):

    ```
    cabal build
    ```

2. Using stack:

    ```
    stack build
    ```
