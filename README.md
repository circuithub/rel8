# Rel8 [![Hackage version](https://img.shields.io/hackage/v/rel8.svg?style=flat)](http://hackage.haskell.org/package/rel8) [![Hackage dependencies](https://img.shields.io/hackage-deps/v/rel8.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=rel8) [![Read The Docs](https://readthedocs.org/projects/rel8/badge/?version=latest)](http://rel8.readthedocs.io/en/latest/?badge=latest) [![Build Status](https://travis-ci.org/ocharles/rel8.svg?branch=master)](https://travis-ci.org/ocharles/rel8)

Welcome to Rel8! Rel8 is an API built on top of the
fantastic [Opaleye](https://hackage.haskell.org/package/opaleye) library to
provide an easy and type-safe way to interact with relational databases.

The main objectives of Rel8 are:

* *Conciseness*: Users using Rel8 should not need to write boiler-plate code. By
  using expressive types, we can provide sufficient information for the compiler
  to infer code whenever possible.

* *Inferrable*: Despite using a lot of type level magic, it should never be a
  requirement that the user must provide a type signature to allow a program to
  compile.

* *Compatible*: Rel8 tries to use the existing Opaleye API as much as possible.

Documentation / tutorials are available online at http://rel8.readthedocs.io
