# a-tree

[![Erlang](https://github.com/AntoineGagne/erl-a-tree/actions/workflows/erlang.yml/badge.svg)](https://github.com/AntoineGagne/erl-a-tree/actions/workflows/erlang.yml)
[![Hex Pm](http://img.shields.io/hexpm/v/a_tree.svg?style=flat)](https://hex.pm/packages/a_tree)
[![Docs](https://img.shields.io/badge/hex-docs-green.svg?style=flat)](https://hexdocs.pm/a_tree/)

The A-Tree data structure is used to evaluate a large amount of boolean expressions as fast as possible. To achieve this, the data structure tries to reuse the intermediary nodes of the incoming expressions to minimize the amount of expressions that have to be evaluated.

This is an Erlang NIF for the Rust crate [a-tree](https://github.com/AntoineGagne/a-tree).

## Dependencies

To be able to build this library, the following dependencies are required:

* `rustc`
* `cargo`
* `rebar3`

## Build

```sh
rebar3 compile
```
