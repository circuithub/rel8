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

  newtype UserId = UserId { getUserId :: Int64 }
    deriving newtype DBType

  newtype OrderId = OrderId { getOrderId :: Int64 }
    deriving newtype DBType

Now we can use ``UserId`` and ``OrderId`` in our Rel8 queries and definitions,
and Haskell will make sure we don't accidentally use an ``OrderId`` when we're
looking up a user.

If you would like to use this approach but can't use generalized newtype
deriving, the same can be achived by using ``mapTypeInformation``::

  instance DBType UserId where
    typeInformation = mapTypeInformation UserId getUserId typeInformation

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
------------------------------------

The ``DBType`` definition for ``OrderStatus`` above is a perfectly reasonable
definition, though it is quite verbose and tedious. Rel8 makes it easy to map
Haskell types that are encoded using ``Read``/``Show`` via the ``ReadShow``
wrapper. An equivalent ``DBType`` definition using ``ReadShow`` is::

  data OrderStatus = PaymentPending | Paid | Packed | Shipped
    deriving stock (Read, Show)
    deriving DBType via ReadShow OrderStatus

Storing structured data with ``JSONEncoded``
--------------------------------------------

It can occasionally be useful to treat PostgreSQL more like a document store,
storing structured documents as JSON objects. Rel8 comes with support for
serializing values into structured representations through the ``JSONEncoded``
and ``JSONBEncoded`` deriving-via helpers.

There usage is very similar to ``ReadShow`` - simply derive ``DBType via
JSONEncoded``, and Rel8 will use ``ToJSON`` and ``FromJSON`` instances (from
``aeson``) to serialize the given type.

For example, a project might use event sourcing with a table of events. Each row
in this table is an event, but this event is stored as a JSON object. We can use
this type with Rel8 by writing::

  data Event = UserCreated UserCreatedData | OrderCreated OrderCreatedData
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON)
    deriving DBType via JSONBEncoded Event

Here we used ``JSONBEncoded``, which will serialize to PostgreSQL ``jsonb``
(binary JSON) type, which is generally the best choice.

``Expr`` and ``Sql``
====================

``Expr``
--------

Now that we've seen how types are bridged between a database and Haskell via
``DBType``, we can start looking at how expressions are modelled. In Rel8, any
expression will be a value of the form ``Expr a``, where ``a`` is the type of
the expression. For example, the SQL expression ``user.id = 42`` is an ``Expr
Bool``, and the subexpressions ``user.id`` and ``42`` might be ``Expr
UserId``\s.

``Expr``\s are usually created by combining other ``Expr``\s together, like
using numeric operations like `+` and `*`, or by quoting Haskell values into
queries with ``lit``. Continuing with the example of ``user.id = 42``, we can
write this in Rel8 as::

  userId ==. lit (UserId 42)

Here it's assumed that a ``userId :: Expr UserId`` expression is already in
scope, and we can compare that against another ``Expr UserId`` using the ``==.``
operator (which is like Haskell's normal ``==`` operator, but lifted to work on
``Expr``). The expression ``lit (UserId 42)`` quotes the Haskell term ``UserId
42`` literally as the SQL expression ``42``. This particular encoding is chosen
because ``UserId`` is simply a ``newtype`` around ``Int64``.

``null``
--------

So far we've only talked about ``DBType``, which represents database types
*excluding* ``null``. Rel8 chooses this encoding because ``null`` is rather
special in SQL, and doesn't really constitute a distinct type. For example,
there is no SQL notion of "stacking" nulls - which is to say a type like ``Maybe
(Maybe UserId))`` has no real analogous type in SQL.

Of course, as ``null`` is pervasive in SQL queries, Rel8 does support ``null`` -
simply wrap up your type in ``Maybe``. ``Nothing`` will be translated as
``null``, and ``Just`` is used to represent non-null values.

Rel8 comes with a set of functions to work with ``null`` values thash should
feel familiar to Haskell programmers:

* ``null`` creates null values (you can also use ``lit Nothing``).
* ``nullify`` turns an ``Expr a`` into a ``Expr (Maybe a)`` (like ``Just``).
* ``nullable`` allows you to eliminate null values, like the ``maybe`` function.
* ``isNull`` and ``isNonNull`` work like ``isNothing`` and ``isJust``,
  respectively.
* ``mapNullable`` is like ``fmap`` for ``Maybe``, and allows you to map over
  non-null values.
* ``liftOpNullable`` is like ``liftA2`` for ``Maybe``, and allows you to combine
  expressions together, given a way to combine non-null expressions.

``SQL`` and null-polymorphic expressions
----------------------------------------

Through the API reference documentation for Rel8, you might encounter the
``Sql`` type class. For example, if we look at the type of ``litExpr``, we
have::

  litExpr :: Sql DBType a => a -> Expr a 

Here ``Sql DBType a`` means that ``a`` can either be literally a type that has
an instance of ``DBType`` (like ``UserId`` or ``Bool``), *or* that same type
wrapped in ``Maybe`` (so ``Maybe UserId`` or ``Maybe Bool``). ``Maybe`` here
encodes the SQL concept of ``null``.

Some functions work regardless of whether or not a value is null, and in these
cases you'll see ``Sql DBType a``. ``Sql`` can be used with any ``DBType``
subtype. For example, the type of ``div`` is::

  div :: Sql DBIntegral a => Expr a -> Expr a -> Expr a 

Which means ``div`` works on any ``DBIntegral a``, including ``Maybe a``.

The ``DBType`` subtypes
=======================

``DBEq``
--------

.. todo::

  Document this

``DBOrd``
---------

.. todo::

  Write this

``DBSemigroup`` and ``DBMonoid``
--------------------------------

.. todo::

  Write this

``DBNum``, ``DBIntegral`` and ``DBFractional``
----------------------------------------------

.. todo::

  Write this

``DBString``
------------

.. todo::

  Write this
