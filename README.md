# Coroutines with Continuations

A Haskell implementation of coroutines built on top of `ContT` and `StateT`. Coroutines are scheduled cooperatively via `yield` and `fork`, with the continuation queue stored in the state layer. Includes a small demo of an animated worm that grows and shrinks using this coroutine system.

## Usage

Run the demo with:

```
nix run
```

## Development

This project uses Nix flakes. Enter the dev shell with:

```
direnv allow
```

Then build or run with cabal:

```
cabal build
cabal run
```
