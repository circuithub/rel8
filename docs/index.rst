Welcome to Rel8!
================

Welcome to Rel8! Rel8 is a Haskell library for interacting with PostgreSQL
databases, built on top of the fantastic `Opaleye
<https://hackage.haskell.org/package/opaleye>`_ library.

The main objectives of Rel8 are:

* *Conciseness*: Users using Rel8 should not need to write boiler-plate code.
  By using expressive types, we can provide sufficient information for the
  compiler to infer code whenever possible.

* *Inferrable*: Despite using a lot of type level magic, Rel8 aims to have
  excellent and predictable type inference.

* *Familiar*: writing Rel8 queries should feel like normal Haskell programming.

.. toctree::
   :caption: Getting Started
   :maxdepth: 2

   tutorial

.. toctree::
   :caption: Rel8 concepts
   :maxdepth: 1

   concepts/dbtype
   concepts/expr
   concepts/tables

.. toctree::
   :caption: Tips and tricks
   :maxdepth: 1

   cookbook


More Resources
==============

* The `Haskell API documentation <https://hackage.haskell.org/package/rel8>`_
  describes how individual functions are types are to be used.

* If you have a question about how to use Rel8, feel free to open a `GitHub
  discussion <https://github.com/circuithub/rel8/discussions>`_.

* If you think you've found a bug, confusing behavior, or have a feature
  request, please raise an issue at `Rel8's issue tracker
  <https://github.com/circuithub/rel8>`_. 
