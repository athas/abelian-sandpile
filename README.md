# Abelian Sandpile model in Futhark

A very straightforward implementation of the [Abelian
Sandpile](https://www.math.cmu.edu/~wes/sandgallery.html) using
[Lys](https://github.com/diku-dk/lys) for visualisation.

Click (or drag) with the mouse to add new sinks.

## Running it

Requires Futhark 0.12.0 (unreleased as of this writing; you'll have to
get it from Git).

```
$ futhark pkg sync
$ make run
```
