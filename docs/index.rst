Welcome to Rel8!
================================

Welcome to Rel8!

Rel8 is a library that builds open the fantastic `Opaleye
<https://hackage.haskell.org/package/opaleye>`_ library to query databases, and
provides a slightly alternative API. The main objectives of Rel8 are:

* *Conciseness*: Users using Rel8 should not need to write boiler-plate code. By
  using expressive types, we can provide sufficient information for the compiler
  to infer code whenever possible.

* *Inferrable*: Despite using a lot of type level magic, it should never be a
  requirement that the user must provide a type signature to allow a program to
  compile.

* *Compatible*: Rel8 tries to use the existing Opaleye API as much as possible.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   tutorial
   differences


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
