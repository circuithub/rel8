Writing queries with ``Query``
==============================

To fetch data from a database, Rel8 allows you to write ``SELECT`` queries using
the ``Query`` monad. While this monad might look a little different from
ordinary SQL, it is equal in expressivity, but due to Haskell's ``do`` notation
we get to benefit from all the means of abstraction available.

Understanding the ``Query`` monad
---------------------------------

Before we look at special functions for working with ``Query``, we'll first take
a moment to understand how the ``Query`` monad works. First, what does the type
``Query a`` *mean*? To be a ``Query a`` means to be a SQL ``SELECT`` query that
selects rows of type ``a``. Usually ``a`` will be an instance of ``Table Expr``,
such as ``Expr Text``, or maybe ``BlogPostComment Expr``.

As ``Query`` is an instance of ``Monad`` means that we already have three
familiar APIs to work with: ``Functor``, ``Applicative``, and ``Monad``.

``Functor Query``
^^^^^^^^^^^^^^^^^

The ``Functor`` instance gives us access to ``fmap``, and its type is::

  fmap :: (a -> b) -> Query a -> Query b

``fmap`` uniformly transforms rows of one type into rows of another type. In
SQL, this is corresponds to a *projection*. For example, if we have a ``Query
(User Expr)``, we might do ``fmap userId`` to transform this into a ``Query
(Expr UserId)``.

``Applicative Query``
^^^^^^^^^^^^^^^^^^^^^

The ``Applicative`` instance for ``Query`` gives us::

  pure :: a -> Query a
  (<*>) :: Query (a -> b) -> Query a -> Query b

``pure`` constructs a ``Query`` that returns exactly one row - a row containing
the ``a`` that was given. This might seem fairly pointless, but it's an
important ``Query`` when compared with ``(<*>)``. The ``<*>`` combines two
``Query``\s by taking their *cartesian product*, followed by a projection that
combines each row into a new row.

One example of using the ``Applicative`` operators is to combine two ``Query``\s
into a tuple::

  pure (,) <*> queryA <*> queryB

``Monad Query``
^^^^^^^^^^^^^^^

The final type class to discuss is ``Monad Query``. ``Monad Query`` has two
methods::

  return :: a -> Query a
  (>>=) :: Query a -> (a -> Query b) -> Query b

``return`` is the same ``pure``, so we won't discuss this further. The much more
interesting operation is ``>>=`` - commonly referred to as "bind". This operator
allows you to *extend* a ``Query`` with a new query. In SQL this is also similar
to a cartesian product, but uses the ``LATERAL`` modifier to allow the second
query to refer to columns from the first.

This extension operator allows you to expand each row in the first query into
zero, one, or many rows, according to the given function. For example, if we
have a database of orders, we might write::

  getAllOrders >>= \order -> getUserById (orderUserId order)

This ``Query`` will return, for each ``Order``, the ``User`` who placed that
order. In this case, this is a one-to-one relationship, so we get back exactly
as many rows as there are orders.

Going in the other direction, we have::

  getAllUsers >>= \user -> getOrdersForUser (userId user)

This is a different query, as we start by fetching all ``User``\s, and for each
user find all ``Order``\s they have placed. This ``Query`` has a different
cardinality, as we're following a one-to-many relationship: any ``User`` may
have zero, one, or many orders.

Haskell has special syntax for working with monads - ``do`` notation. ``do``
notation allows you to write these queries in a simpler form, where we don't
have to introduce functions. Expanding on the latter query, we could write::

  do user  <- getAllUsers
     order <- getOrdersForUser (user)
     return (user, order)

Now we have a query that, for each ``User``, fetches all orders for that user.
The final ``return`` means that for each ``User`` and ``Order``, we'll return a
single row.

Selecting rows from tables
--------------------------

With the more theoretical side of ``Query`` considered, we can start looking at
the more pragmatic side, and how ``Query`` can express some common SQL idioms.

First, one of the most common operations is to select all rows from a *base
table*. In SQL, this is a ``SELECT * FROM x`` query, and in Rel8 we use ``each``
with a ``TableSchema``.

Limit and offset
----------------

The SQL ``LIMIT`` and ``OFFSET`` keywords are available in Rel8 as ``limit``
and ``offset``. Note that, like SQL, the order of these operations matters.
Usually, the correct thing to do is to first apply an offset with ``offset``,
and then use ``limit`` to limit the number of rows returned::

  limit n . offset m . orderBy anOrdering

These operations are similar to Haskell's ``take`` and ``drop`` operations.

Filtering queries
-----------------

.. todo::

  Write this

Inner joins
-----------

.. todo::

  Write this

Left (outer) joins with ``optional``
------------------------------------

.. todo::

  Write this

Ordering results
----------------

.. todo::

  Write this

Aggregating queries
-------------------

.. todo::

  Write this

Set operations
--------------

.. todo::

  Write this
