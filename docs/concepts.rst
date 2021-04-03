This documentation covers the main concepts that are used throughout Rel8.

``DBType``
==========

The ``DBType`` type class provides a bridge between database expression values
and Haskell values. Rel8 comes with stock instances for most types that come
predefined with PostgreSQL, such as `int4` (which is represented as by
``Data.Int.Int32``) and timestamptz (which is ``UTCTime`` from ``Data.Time``).
This means that you need at least one ``DBType`` instance per database type,
though most of these primitive ``DBType``\s should already be available.

Combining ``newtype`` and ``DBType``
------------------------------------

You can define new instances of ``DBType`` by using Haskell "generalized newtype
deriving" strategy. This is useful when you have a common database type, but
need to interpret this type differently in different contexts. A very common
example here is with auto-incrementing ``id`` counters. In PostgreSQL, it's
common for a table to have a primary key that uses the ``serial`` type, which
means the key is an ``int8`` with a default value for ``INSERT``. In Rel8, we
could use ``Int64`` (as ``Int64`` is the ``DBType`` instance for ``int8``), but
we can be even clearer if we make a ``newtype`` for each *type* of id.

If our database has users and orders, these tables might both have ids, but they
are clearly not meant to be treated as a common type. Instead, we can make these
types clearly different by writing::

  newtype UserId = UserId Int64
    deriving newtype DBType

  newtype OrderId = OrderId Int64
    deriving newtype DBType

Now we can use ``UserId`` and ``OrderId`` in our Rel8 queries and definitions,
and Haskell will make sure we don't accidentally use an ``OrderId`` when we're
looking up a user.

Parsing with ``DBType``
-----------------------

``DBType``\s can also *refine* database types with parsing, which allows you to
map more structured Haskell types to a PostgreSQL database. This allows you to
use the capabalities of Haskell's rich type system to make it harder to write
incorrect queries. For example, we might have a database where we need to store
the status of an order. In Haskell, we might write:

.. code-block:: haskell

  data OrderStatus = PaymentPending | Paid | Packed | Shipped

In our PostgreSQL we have a few choices, but for now we'll assume that they are
stored as ``text`` values.

In order to use this type in Rel8 queries, we need to write an instance of
``DBType`` for ``OrderStatus``. One approach is to use ``parseTypeInformation``,
which allows you to refine an existing ``DBType``::

  instance DBType OrderStatus where
    typeInformation = parseTypeInformation parser printer typeInformation
      where
        parser :: Text -> Either String OrderStatus
        parser "PaymentPending" = Right PaymentPending
        parser "Paid" = Right Paid
        parser "Packed" = Right Packed
        parser "Shipped" = Right Shipped
        parser other = Left $ "Unknown OrderStatus: " <> unpack other

        printer :: OrderStatus -> Text
        printer PaymentPending = "PaymentPending"
        printer Paid = "Paid"
        printer Packed = "Packed"
        printer Shipped = "Shipped"

Deriving ``DBType`` via ``ReadShow``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is a perfectly reasonable definition, though it is quite verbose and
tedious. Rel8 makes it easy to map Haskell types that are encoded using
``Read``/``Show`` via the ``ReadShow`` wrapper. An equivalent ``DBType``
definition using ``ReadShow`` is::

  data OrderStatus = PaymentPending | Paid | Packed | Shipped
    deriving stock (Read, Show)
    deriving DBType via ReadShow OrderStatus
